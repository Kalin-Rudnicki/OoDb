package oo_db.db

import java.io.{File, RandomAccessFile}

import oo_db.db.BTree.RemoveResult
import scalaz.std.option.optionSyntax._
import oo_db.db.nodes._
import sun.rmi.runtime.Log

import scala.annotation.tailrec

class BTree(private val io: IoManager) {
	
	// =====| General |=====
	
	def close: Unit =
		io.close
	
	def showStats: Unit =
		io.showStats
	
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
				handleInsert(
					io.readLeafNode(seek).insert(io.order, io.nextFreePos, key, value)
				)
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
			io.insertRootNode(node)
		}
	}
	
	def remove(key: Long): Option[Long] = {
		// Helpers
		/**
		  * None =>
		  * 	No deletion
		  * Some((v, None, None)) =>
		  * 	deleted key, which pointed to v
		  * 	at this point in the stack, all merging has been handled
		  * 	also, no keys need to be changed
		  * Some((v, Some(n), None)) =>
		  * 	deleted key, which pointed to v
		  * 	after deletion cascaded from child, it is now too small, and needs to be handled
		  * 	also, no keys need to be changed
		  * Some((v, None, Some(k)) =>
		  * 	deleted key, which pointed to v
		  * 	at this point in the stack, all merging has been handled
		  * 	key was the smallest in its leaf, and needs a new reference internally
		  * 		this should only be Some if the first element is removed
		  * Some((v, Some(n), Some(k)) =>
		  * 	both previous cases need to be handled
		  */
		/**
		  * @param depth  : How deep you are into the tree
		  * @param minKey : The last key you went to the "right" of (might be 1, or several levels up) (or even never if you hit the leftmost child leaf)
		  * @param pos    : Position of the node you are currently searching
		  * @param left   : The pos of the node to the left of you
		  * @param right  : The (key, pos) of the node to the right of you
		  * @return
		  */
		def loop(depth: Int, minKey: Option[Long], pos: Long, left: Option[Long], right: Option[(Long, Long)]): RemoveResult = { // TODO : More? Less?
			if (depth < io.getHeight) { // Internal
				val node = io.readInternalNode(pos)
				val (nMinKey, nPos, nLeft, nRight) = node.childAndNeighbors(key)
				val res = loop(depth + 1, nMinKey, nPos, nLeft, nRight)
				
				println(s"=====| $depth |=====")
				println
				println(s" minKey: $minKey")
				println(s"    pos: $pos")
				println(s"   left: $left")
				println(s"  right: $right")
				println
				println(s"nMinKey: $nMinKey")
				println(s"   nPos: $nPos")
				println(s"  nLeft: $nLeft")
				println(s" nRight: $nRight")
				println
				println(s"   res: $res")
				println
				
				RemoveResult.NoAction // TODO
			}
			else {
				io.readLeafNode(pos).remove(key) match {
					case None =>
						RemoveResult.NoAction
					case Some((v, n, b)) =>
						
						(left, right) match {
							case (Some(l), _) =>
								val lNode = io.readLeafNode(l)
								if (lNode.keys.length > io.minKeys) {
									val (lNode2, (nK, nV)) = lNode.borrowFromEnd
									val n2 = LeafNode(n.pos, nK :: n.keys, nV :: n.values, n.nextLeaf)
									io.writeNode(lNode2)
									io.writeNode(n2)
									RemoveResult.BorrowedLeft(v, n.keys.head, lNode2, n2)
								}
								else
									??? // TODO : merge left
							case (_, Some(r)) =>
								val rNode = io.readLeafNode(r._1)
								if (rNode.keys.length > io.minKeys) {
									val rNode2 = LeafNode(rNode.pos, rNode.keys.tail, rNode.values.tail, rNode.nextLeaf)
									val n2 = LeafNode(n.pos, n.keys ::: rNode.keys.head :: Nil, n.values ::: rNode.values.head :: Nil, n.nextLeaf)
									io.writeNode(rNode2)
									io.writeNode(n2)
									RemoveResult.BorrowedRight(v, rNode.keys.head, rNode2, n2)
								}
								else
									??? // merge right
							case _ => // I am root, should only be returning directly to call at "Action"
								if (n.keys.isEmpty) {
									io.deleteNode(n)
									io.setRoot(None)
									io.setHeight(0)
									RemoveResult.RootRemoved(v)
								}
								else {
									io.writeNode(n)
									RemoveResult.RootReduced(v)
								}
						}
				}
			}
		}
		
		
		// Action
		if (io.getHeight > 0)
			loop(1, None, io.getRoot, None, None).value
		else {
			println("Action-3")
			None
		}
	}
	
}

object BTree {
	// Constants
	val MAGIC_NUMBER: Int = 28366439
	val MIN_ORDER: Int = 3
	
	val MAGIC_NUMBER_POSITION: Long = 0L // Int
	val ORDER_POSITION: Long = 4L // Int
	val HEIGHT_POSITION: Long = 8L // Int
	val ROOT_POSITION: Long = 12L // Long
	val FREE_LIST_POSITION: Long = 20L // Long
	
	val FREE_LIST_MARKER: Byte = 0
	val INTERNAL_NODE_MARKER: Byte = 1
	val LEAF_NODE_MARKER: Byte = 2
	
	// ...
	
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
	
	
	sealed trait RemoveResult {
		def value: Option[Long] // TODO : Is this necessary?
	}
	
	abstract class RemoveResultV(val v: Long) extends RemoveResult {
		override def value: Option[Long] = v.some
	}
	
	object RemoveResult {
		
		case object NoAction extends RemoveResult {
			override def value: Option[Long] = None
		}
		
		case class RootRemoved(_v: Long) extends RemoveResultV(_v)
		
		case class RootReduced(_v: Long) extends RemoveResultV(_v)
		
		case class BorrowedLeft(_v: Long, nPrevMin: Long, newLeft: Node[_], newNode: Node[_]) extends RemoveResultV(_v)
		
		case class BorrowedRight(_v: Long, rPrevMin: Long, newRight: Node[_], newNode: Node[_]) extends RemoveResultV(_v)
		
	}
	
	
}
