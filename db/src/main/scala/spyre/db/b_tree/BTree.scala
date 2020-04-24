package spyre.db.b_tree

import java.io.{File, RandomAccessFile}

import spyre.db.b_tree.nodes._
import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class BTree(private val io: IoManager) {
	
	import BTree.{RemoveResult => R}
	import BTree.{ParentChange => P}
	
	// =====| General |=====
	
	def size: Int =
		io.getSize
	
	def close: Unit =
		io.close
	
	def showStats: Unit =
		io.showStats
	
	def writeImage(path: String, message: Option[String] = None): Unit =
		io.writeImage(path, message)
	
	// =====| Operations |=====
	
	def get(key: Long): Option[Long] = {
		@tailrec
		def find(depth: Int, seek: Long): Option[Long] =
			if (depth < io.getHeight)
				find(depth + 1, io.readInternalNode(seek).childPos(key))
			else
				io.readLeafNode(seek).find(key)
		
		if (io.getHeight == 0)
			None
		else
			find(1, io.getRoot)
	}
	
	def insert(key: Long, value: Long): Unit = {
		// Helpers
		def handleInsert(res: Option[(Node[_], Option[(Long, Node[_])])]): Option[(Long, Long)] = res match {
			case None =>
				None
			case Some((n1, None)) =>
				io.writeNode(n1)
				None
			case Some((n1, Some((minKey, n2)))) =>
				io.writeNode(n1)
				io.insertNewNode(n2)
				(minKey, n2.pos).some
		}
		
		def loop(depth: Int, seek: Long): Option[(Long, Long)] =
			if (depth < io.getHeight) {
				val internalNode: InternalNode = io.readInternalNode(seek)
				loop(depth + 1, internalNode.childPos(key)) match {
					case None =>
						None
					case Some((newKey, newChild)) =>
						handleInsert(
							internalNode.insert(io.order, io.nextFreePos, newKey, newChild)
						)
				}
			}
			else {
				val res = io.readLeafNode(seek).insert(io.order, io.nextFreePos, key, value)
				if (res.isDefined && res.get._1)
					io.setSize(io.getSize + 1)
				handleInsert(res.map(r => (r._2, r._3)))
			}
		
		// Action
		if (io.getHeight > 0)
			loop(1, io.getRoot) match {
				case None =>
				case Some((rightMin, pos)) =>
					val newRoot: InternalNode = InternalNode(io.nextFreePos, List(rightMin), List(io.getRoot, pos))
					io.insertRootNode(newRoot)
			}
		else {
			val node: LeafNode = new LeafNode(io.nextFreePos, List(key), List(value), 0L)
			io.setSize(1)
			io.insertRootNode(node)
		}
	}
	
	def remove(key: Long): Option[Long] = {
		// Helpers
		def loop(depth: Int, minKey: Option[Long], pos: Long, left: Option[Long], right: Option[(Long, Long)]): R = { // TODO : More? Less?
			if (depth < io.getHeight) { // Internal
				val n0 = io.readInternalNode(pos)
				val (nMinKey, nPos, nLeft, nRight) = n0.childAndNeighbors(key)
				val res = loop(depth + 1, nMinKey.fold(minKey)(m => m.some), nPos, nLeft, nRight)
				
				res match {
					case R.NoAction =>
						res
					case _: R.Handled =>
						res
					case r: R.Cascade =>
						n0.handle(r.changes).foreach(n1 => {
							if (n1.keys.length < io.minKeys)
								(left, right) match {
									case (Some(l), _) =>
										val lNode = io.readInternalNode(l)
										if (lNode.keys.length > io.minKeys) { // Borrow Left
											val (lNode2, (nK, nV)) = lNode.borrowFromEnd
											val n2 = InternalNode(n1.pos, minKey.get :: n1.keys, nV :: n1.children)
											io.writeNode(lNode2)
											io.writeNode(n2)
											r.changes.put(minKey.get, P.Replace(nK))
										}
										else { // Merge Left
											val lNode2 = InternalNode(lNode.pos, lNode.keys ::: minKey.get :: n1.keys, lNode.children ::: n1.children)
											io.writeNode(lNode2)
											io.deleteNode(n1)
											r.changes.put(minKey.get, P.Delete)
										}
									case (_, Some((rKey, rVal))) =>
										val rNode = io.readInternalNode(rVal)
										if (rNode.keys.length > io.minKeys) { // Borrow Right
											val rNode2 = InternalNode(rNode.pos, rNode.keys.tail, rNode.children.tail)
											val n2 = InternalNode(n1.pos, n1.keys ::: rKey :: Nil, n1.children ::: rNode.children.head :: Nil)
											io.writeNode(rNode2)
											io.writeNode(n2)
											r.changes.put(rKey, P.Replace(rNode.keys.head))
										}
										else { // Merge Right
											val n2 = InternalNode(n1.pos, n1.keys ::: rKey :: rNode.keys, n1.children ::: rNode.children)
											io.writeNode(n2)
											io.deleteNode(rNode)
											r.changes.put(rKey, P.Delete)
										}
									case _ => // I am root, should only be returning directly to call at "Action"
										if (n1.keys.isEmpty) {
											io.deleteNode(n1)
											io.setRoot(n1.children.head.some)
											io.setHeight(io.getHeight - 1)
										}
										else
											io.writeNode(n1)
								}
							else
								io.writeNode(n1)
						})
						r.pass
				}
			}
			else { // Leaf
				val n0 = io.readLeafNode(pos)
				n0.remove(key) match {
					case None =>
						R.NoAction
					case Some((v, n1, b)) =>
						io.setSize(io.getSize - 1)
						val map: MMap[Long, P] = MMap()
						if (b && n1.keys.nonEmpty)
							minKey.foreach(m => map.put(m, P.Replace(n1.keys.head))) // doesnt add anything if it is the leftmost-leaf
						
						if (n1.keys.length < io.minKeys)
							(left, right) match {
								case (Some(l), _) =>
									val lNode = io.readLeafNode(l)
									if (lNode.keys.length > io.minKeys) { // Borrow Left
										val (lNode2, (nK, nV)) = lNode.borrowFromEnd
										val n2 = LeafNode(n1.pos, nK :: n1.keys, nV :: n1.values, n1.nextLeaf)
										io.writeNode(lNode2)
										io.writeNode(n2)
										map.put(minKey.get, P.Replace(n2.keys.head))
									}
									else { // Merge Left
										val lNode2 = LeafNode(lNode.pos, lNode.keys ::: n1.keys, lNode.values ::: n1.values, n1.nextLeaf)
										io.writeNode(lNode2)
										io.deleteNode(n1)
										map.put(minKey.get, P.Delete)
									}
									R.Cascade(v, map)
								case (_, Some((rKey, rVal))) =>
									val rNode = io.readLeafNode(rVal)
									if (rNode.keys.length > io.minKeys) { // Borrow Right
										val rNode2 = LeafNode(rNode.pos, rNode.keys.tail, rNode.values.tail, rNode.nextLeaf)
										val n2 = LeafNode(n1.pos, n1.keys ::: rNode.keys.head :: Nil, n1.values ::: rNode.values.head :: Nil, n1.nextLeaf)
										io.writeNode(rNode2)
										io.writeNode(n2)
										map.put(rKey, P.Replace(rNode2.keys.head))
									}
									else { // Merge Right
										val n2 = LeafNode(n1.pos, n1.keys ::: rNode.keys, n1.values ::: rNode.values, rNode.nextLeaf)
										io.writeNode(n2)
										io.deleteNode(rNode)
										map.put(rKey, P.Delete)
									}
									R.Cascade(v, map)
								case _ => // I am root, should only be returning directly to call at "Action"
									if (n1.keys.isEmpty) {
										io.deleteNode(n1)
										io.setRoot(None)
										io.setHeight(0)
										R.Handled(v)
									}
									else {
										io.writeNode(n1)
										R.Handled(v)
									}
							}
						else {
							io.writeNode(n1)
							R.Cascade(v, map).pass
						}
				}
			}
		}
		
		// Action
		if (io.getHeight > 0)
			loop(1, None, io.getRoot, None, None).value
		else {
			None
		}
	}
	
}

object BTree {

	// Constants
	val MAGIC_NUMBER: Int = 28366439
	val MIN_ORDER: Int = 3

	// Getting a BTree instance

	def create(order: Int, path: String): BTree = {
		if (order < MIN_ORDER)
			throw new IllegalArgumentException(s"order $order is less than min $MIN_ORDER")
		
		val file: File = new File(path)
		if (file.exists())
			throw new IllegalArgumentException(s"File already exists at path $path")
		
		file.getParentFile.mkdirs
		
		val bTreeFile: RandomAccessFile = new RandomAccessFile(file, "rw")
		bTreeFile.writeInt(MAGIC_NUMBER) // Magic Number
		bTreeFile.writeInt(order) // Order
		bTreeFile.writeInt(0) // Height
		bTreeFile.writeInt(0) // Size
		bTreeFile.writeLong(0L) // Root
		bTreeFile.writeLong(0L) // Free List
		bTreeFile.close
		
		new BTree(new IoManager(path))
	}
	
	def load(path: String): BTree = {
		val file: File = new File(path)
		if (!file.exists())
			throw new IllegalArgumentException(s"File does not exist at path $path")
		
		new BTree(new IoManager(path))
	}
	
	// ParentAction
	
	sealed trait ParentChange
	
	object ParentChange {
		
		case object Delete extends ParentChange
		
		case class Replace(replace: Long) extends ParentChange
		
	}
	
	// RemoveResult
	
	sealed trait RemoveResult {
		def value: Option[Long]
	}
	
	object RemoveResult {
		
		case object NoAction extends RemoveResult {
			override def value: Option[Long] = None
		}
		
		sealed abstract class Action(_v: Long) extends RemoveResult {
			override def value: Option[Long] = _v.some
		}
		
		case class Handled(v: Long) extends Action(v)
		
		case class Cascade(v: Long, changes: MMap[Long, ParentChange]) extends Action(v) {
			def pass: RemoveResult =
				if (changes.isEmpty)
					Handled(v)
				else
					this
		}
		
	}
	
	
}
