package oo_db

import java.io.File
import java.io.RandomAccessFile

import scala.annotation.tailrec

import scalaz.std.option.optionSyntax._

class BTree(private val io: BTree.IoManager) {
	
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
		def handleInsert(res: Option[(BTree.Node, Option[(Long, BTree.Node)])]): Option[(Long, Long)] = res match {
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
				val internalNode: BTree.InternalNode = io.readInternalNode(seek)
				loop(depth + 1, internalNode.childPos(key)) match {
					case None =>
						None
					case Some((newKey, newChild)) =>
						handleInsert(
							internalNode.insert(io.getOrder, io.nextFreePos, newKey, newChild)
						)
				}
			}
			else {
				handleInsert(
					io.readLeafNode(seek).insert(io.getOrder, io.nextFreePos, key, value)
				)
			}
		
		if (io.getHeight == 0) {
			// Create root node, write it, and save root/height
			val node: BTree.LeafNode = new BTree.LeafNode(io.nextFreePos, List(key), List(value), 0L)
			io.insertRootNode(node)
		}
		else
			loop(1, io.getRoot) match {
				case None =>
				case Some((rightMin, pos)) =>
					val newRoot: BTree.InternalNode = BTree.InternalNode(io.nextFreePos, List(rightMin), List(io.getRoot, pos))
					io.insertRootNode(newRoot)
			}
		// io.showStats
	}
	
	def remove(key: Long): Unit = {
		// TODO
		throw new UnsupportedOperationException
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
	
	// =====| Internals manager |=====
	
	class IoManager(path: String) {
		
		// =====| Constructor |=====
		
		private val bTreeFile: RandomAccessFile = new RandomAccessFile(path, "rw")
		
		bTreeFile.seek(0)
		if (bTreeFile.readInt() != BTree.MAGIC_NUMBER)
			throw new RuntimeException(s"'$path' is not a bTree")
		
		val order: Int = bTreeFile.readInt()
		if (order < BTree.MIN_ORDER)
			throw new RuntimeException(s"Something has gone seriously wrong... (order: $order)")
		
		private var height: Int = bTreeFile.readInt()
		private var root: Option[Long] = bTreeFile.readLong.some.flatMap(r => {
			if (r == 0L)
				None
			else
				r.some
		})
		private var freeListHead: Option[Long] = bTreeFile.readLong.some.flatMap(f => {
			if (f == 0L)
				None
			else
				f.some
		})
		
		def close: Unit =
			bTreeFile.close
		
		def showStats: Unit = {
			bTreeFile.seek(0L)
			val _magic = bTreeFile.readInt()
			val _order = bTreeFile.readInt()
			val _height = bTreeFile.readInt()
			val _root = bTreeFile.readLong()
			val _freeList = bTreeFile.readLong()
			
			println
			println("=====| BTree Stats |=====")
			println(s"magicNumber: ${_magic}")
			println(s"      order: ${_order}")
			println(s"     height: ${_height}")
			println(s"       root: ${_root}")
			println(s"   freeList: ${_freeList}")
			println(s"     length: ${bTreeFile.length}")
			println
			
			while (bTreeFile.getFilePointer < bTreeFile.length) {
				val pos: Long = bTreeFile.getFilePointer
				val t: Byte = bTreeFile.readByte
				val values: List[Long] = 1.to(order).map(_ => bTreeFile.readLong()).toList
				val keys: List[Long] = 2.to(order).map(_ => bTreeFile.readLong()).toList
				
				val prefix = s"> ${pos.toString.reverse.padTo(10, ' ').reverse} / 0x${pos.toHexString.toUpperCase.reverse.padTo(8, '0').reverse}"
				t match {
					case FREE_LIST_MARKER =>
						println(s"$prefix => FREE_LIST(${values.head.toHexString})")
					case INTERNAL_NODE_MARKER =>
						println(s"$prefix => InternalNode($keys, $values)")
					case LEAF_NODE_MARKER =>
						println(s"$prefix => LeafNode($keys, ${values.reverse.tail.reverse}, ${values.last})")
				}
			}
			println("=====| EOF |=====")
		}
		
		// =====| File Attributes |=====
		
		// Read
		
		def nextFreePos: Long = {
			freeListHead.getOrElse(bTreeFile.length)
		}
		
		def getOrder: Int =
			order
		
		def getHeight: Int =
			height
		
		def getRoot: Long =
			root.getOrElse(0L)
		
		// Write
		
		private def setRoot(r: Option[Long]): Unit = {
			bTreeFile.seek(ROOT_POSITION)
			bTreeFile.writeLong(r.getOrElse(0L))
			root = r
		}
		
		private def setFreeList(f: Option[Long]): Unit = {
			bTreeFile.seek(FREE_LIST_POSITION)
			bTreeFile.writeLong(f.getOrElse(0L))
			freeListHead = f
		}
		
		private def setHeight(h: Int): Unit = {
			bTreeFile.seek(HEIGHT_POSITION)
			bTreeFile.writeInt(height)
			height = h
		}
		
		// =====| Nodes |=====
		
		// Read
		
		private def readData: List[Long] = {
			// TODO : This will probably not work probably on LE machines
			@tailrec
			def bytesToLongs(c: Int, t: Long, bytes: List[Byte], pLongs: List[Long]): List[Long] = bytes match {
				case Nil =>
					if (c == 0)
						(t :: pLongs).reverse
					else
						throw new RuntimeException("Not a multiple of 8")
				case bH :: bT =>
					val nL: Long = (t << 8) | bH
					if (c == 0)
						bytesToLongs(7, 0, bT, nL :: pLongs)
					else
						bytesToLongs(c - 1, nL, bT, pLongs)
			}
			
			val array: Array[Byte] = Array.ofDim(order * 16 - 1)
			bTreeFile.readFully(array)
			bytesToLongs(7, 0, array.toList, Nil)
		}
		
		def readInternalNode(pos: Long): InternalNode = {
			bTreeFile.seek(pos)
			if (bTreeFile.readByte != INTERNAL_NODE_MARKER)
				throw new RuntimeException("Tried to read invalid InternalNode")
			
			InternalNode(pos, order, readData)
		}
		
		def readLeafNode(pos: Long): LeafNode = {
			bTreeFile.seek(pos)
			if (bTreeFile.readByte != LEAF_NODE_MARKER)
				throw new RuntimeException("Tried to read invalid LeafNode")
			
			LeafNode(pos, order, readData)
		}
		
		// Write
		
		// TODO : 2nd root split is not working properly
		def insertRootNode(node: Node): Unit = {
			insertNewNode(node)
			setRoot(node.pos.some)
			setHeight(height + 1)
		}
		
		/**
		  * Will add a node at the next free point,
		  * if this nodes pos was not correctly set to that point,
		  * an error will be thrown
		  */
		def insertNewNode(node: Node): Unit = {
			val (insertAt, popFree): (Long, Boolean) = freeListHead.fold((bTreeFile.length, false))(f => (f, true))
			
			if (node.pos != insertAt)
				throw new RuntimeException("Attempt to insert node new node with invalid 'pos'")
			
			bTreeFile.seek(insertAt)
			
			val nextFree: Option[Long] =
				if (popFree) {
					// TODO : Is this correct?
					bTreeFile.seek(insertAt + 1)
					val tmp = bTreeFile.readLong.some.flatMap(f => {
						if (f == 0L)
							None
						else
							f.some
					})
					bTreeFile.seek(insertAt)
					tmp
				}
				else
					None
			
			writeNode(node)
			
			if (popFree)
				setFreeList(nextFree)
		}
		
		def writeNode(node: BTree.Node): Unit = {
			bTreeFile.seek(node.pos)
			node match {
				case _: InternalNode => bTreeFile.writeByte(INTERNAL_NODE_MARKER)
				case _: LeafNode => bTreeFile.writeByte(LEAF_NODE_MARKER)
			}
			
			node.toList(order).foreach(bTreeFile.writeLong)
		}
		
	}
	
	// =====| Nodes |=====
	
	object NodeUtils {
		
		@tailrec
		def takeAtMax(max: Int, done: Boolean, stopAt0: Boolean, list: List[Long], prev: List[Long]): (List[Long], List[Long]) =
			if (max > 0)
				if (done)
					takeAtMax(max - 1, done, stopAt0, list.tail, prev)
				else
					list match {
						case 0 :: tL if stopAt0 =>
							takeAtMax(max - 1, true, stopAt0, tL, prev)
						case hL :: tL =>
							takeAtMax(max - 1, done, stopAt0, tL, hL :: prev)
					}
			else
				(prev.reverse, list)
		
		@tailrec
		def takeFirst(c: Int, list: List[Long], prev: List[Long] = Nil): (List[Long], List[Long]) =
			if (c > 0)
				takeFirst(c - 1, list.tail, list.head :: prev)
			else
				(prev.reverse, list)
		
	}
	
	sealed trait Node {
		
		def pos: Long
		
		def keys: List[Long]
		
		def size: Int
		
		def toList(order: Int): List[Long]
		
		/**
		  * ====================
		  * This method does not handle recursive inserting behavior,
		  * only the insertion of the specified k/v pair, and notifies the caller
		  * of the outcome, which is there responsibility to handle
		  * ====================
		  *
		  * None =>
		  * 	There were no changes to this node based on the insertion
		  * Option(n1, None) =>
		  * 	There was a change to the node, and it needs to be re-written to disk,
		  * 	but it was not split, so no further changes are necessary
		  * Option(n1, Option(m, n2)) =>
		  * 	There was a change to the node, and it needs to be re-written to disk.
		  * 	It was also split, so the addition needs to be added with 'm' as the key,
		  * 	and 'n2.pos' as the value
		  */
		def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])]
		
		@tailrec
		protected final def afterInsert(key: Long, value: Long, k: List[Long], v: List[Long], pK: List[Long], pV: List[Long]): Option[(List[Long], List[Long])] = ((k, v): @unchecked) match {
			case (Nil, Nil) =>
				(
					(key :: pK).reverse,
					(value :: pV).reverse
				).some
			case (hK :: tK, hV :: tV) =>
				if (key < hK)
					(
						pK.reverse ++ (key :: k),
						pV.reverse ++ (value :: v)
					).some
				else if (key == hK)
					(
						pK.reverse ++ k,
						pV.reverse ++ (value :: tV)
					).some
				else
					afterInsert(key, value, tK, tV, hK :: pK, hV :: pV)
		}
		
	}
	
	// InternalNode
	
	case class InternalNode(pos: Long, keys: List[Long], children: List[Long]) extends Node {
		// println(s"# InternalNode($pos, $keys, $children)")
		
		override def size: Int =
			children.size
		
		override def toList(order: Int): List[Long] =
			if (size < order)
				children.padTo(order, 0L) ++ keys.padTo(order - 1, 0L)
			else
				children ++ keys
		
		override def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])] = {
			afterInsert(key, value, keys, children.tail, Nil, children.head :: Nil) match {
				case None =>
					None
				case Some((newKeys, newValues)) =>
					if (newKeys.length < order)
						(
							new InternalNode(pos, newKeys, newValues),
							None
						).some
					else {
						val (keys1, passBackVal :: keys2) = NodeUtils.takeFirst(order / 2, newKeys)
						val (values1, values2) = NodeUtils.takeFirst(order / 2 + 1, newValues)
						(
							new InternalNode(pos, keys1, values1),
							(
								passBackVal,
								new InternalNode(freeListStart, keys2, values2)
							).some
						).some
					}
			}
		}
		
		def childPos(key: Long): Long = {
			@tailrec
			def loop(k: List[Long], c: List[Long]): Long = ((k, c): @unchecked) match {
				case (Nil, hC :: Nil) =>
					hC
				case (hK :: tK, hC :: tC) =>
					if (key < hK)
						hC
					else
						loop(tK, tC)
			}
			
			loop(keys, children)
		}
		
	}
	
	object InternalNode {
		
		def apply(pos: Long, order: Int, list: List[Long]): InternalNode = {
			val (children, keyStart) = NodeUtils.takeAtMax(order, false, true, list, Nil)
			val (keys, _) = NodeUtils.takeAtMax(children.length - 1, false, false, keyStart, Nil)
			new InternalNode(pos, keys, children)
		}
		
	}
	
	// LeafNode
	
	case class LeafNode(pos: Long, keys: List[Long], values: List[Long], nextLeaf: Long) extends Node {
		// println(s"# LeafNode($pos, $keys, $values, $nextLeaf)")
		
		override def size: Int =
			values.size + 1
		
		override def toList(order: Int): List[Long] =
			values.padTo(order - 1, 0L) ++ (nextLeaf :: keys).padTo(order, 0L)
		
		
		override def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])] = {
			afterInsert(key, value, keys, values, Nil, Nil) match {
				case None =>
					None
				case Some((newKeys, newValues)) =>
					if (newKeys.length < order)
						(
							new LeafNode(pos, newKeys, newValues, nextLeaf),
							None
						).some
					else {
						val (keys1, keys2) = NodeUtils.takeFirst((order + 1) / 2, newKeys)
						val (values1, values2) = NodeUtils.takeFirst((order + 1) / 2, newValues)
						val passBackVal = keys2.head
						(
							new LeafNode(pos, keys1, values1, freeListStart),
							(
								passBackVal,
								new LeafNode(freeListStart, keys2, values2, nextLeaf)
							).some
						).some
					}
			}
		}
		
		def find(key: Long): Option[Long] = {
			@tailrec
			def loop(k: List[Long], v: List[Long]): Option[Long] = ((k, v): @unchecked) match {
				case (Nil, Nil) =>
					None
				case (hK :: tK, hV :: tV) =>
					if (key == hK)
						hV.some
					else
						loop(tK, tV)
			}
			
			loop(keys, values)
		}
		
	}
	
	object LeafNode {
		
		def apply(pos: Long, order: Int, list: List[Long]): LeafNode = {
			val (values, nextLeaf :: keyStart) = NodeUtils.takeAtMax(order - 1, false, true, list, Nil)
			val (keys, _) = NodeUtils.takeAtMax(values.length, false, false, keyStart, Nil)
			new LeafNode(pos, keys, values, nextLeaf)
		}
		
	}
	
}
