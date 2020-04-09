package oo_db.db

import java.io.File

import scalaz.std.option.optionSyntax._
import oo_db.db.nodes._
import oo_db.utils.{Bytable, BytableRAF}

class IoManager(path: String) {
	
	// =====| Constructor |=====
	
	private val bTreeFile: BytableRAF = new BytableRAF(new File(path), "rw")
	
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
				case BTree.FREE_LIST_MARKER =>
					println(s"$prefix => FREE_LIST(${values.head.toHexString})")
				case BTree.INTERNAL_NODE_MARKER =>
					println(s"$prefix => InternalNode($keys, $values)")
				case BTree.LEAF_NODE_MARKER =>
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
		bTreeFile.seek(BTree.ROOT_POSITION)
		bTreeFile.writeLong(r.getOrElse(0L))
		root = r
	}
	
	private def setFreeList(f: Option[Long]): Unit = {
		bTreeFile.seek(BTree.FREE_LIST_POSITION)
		bTreeFile.writeLong(f.getOrElse(0L))
		freeListHead = f
	}
	
	private def setHeight(h: Int): Unit = {
		bTreeFile.seek(BTree.HEIGHT_POSITION)
		bTreeFile.writeInt(height)
		height = h
	}
	
	// =====| Nodes |=====
	
	// Read
	
	private def readData: List[Long] =
		bTreeFile.readBytable[Long](order * 2 - 1)
	
	
	def readInternalNode(pos: Long): InternalNode = {
		bTreeFile.seek(pos)
		if (bTreeFile.readByte != BTree.INTERNAL_NODE_MARKER)
			throw new RuntimeException("Tried to read invalid InternalNode")
		
		InternalNode(pos, order, readData)
	}
	
	def readLeafNode(pos: Long): LeafNode = {
		bTreeFile.seek(pos)
		if (bTreeFile.readByte != BTree.LEAF_NODE_MARKER)
			throw new RuntimeException("Tried to read invalid LeafNode")
		
		LeafNode(pos, order, readData)
	}
	
	// Write
	
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
			throw new RuntimeException("Attempt to insert node new node with invalid 'pos', not at start of free list")
		
		bTreeFile.seek(insertAt)
		
		val nextFree: Option[Long] =
			if (popFree) {
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
	
	def writeNode(node: Node): Unit = {
		bTreeFile.seek(node.pos)
		node match {
			case _: InternalNode => bTreeFile.writeByte(BTree.INTERNAL_NODE_MARKER)
			case _: LeafNode => bTreeFile.writeByte(BTree.LEAF_NODE_MARKER)
		}
		
		bTreeFile.writeBytable[Long](node.toList(order))
	}
	
	// NOT-SURE
	def deleteNode(node: Node, zeroOut: Boolean = false): Unit = {
		bTreeFile.seek(node.pos)
		val size = (order - 1) * 16
		if (zeroOut)
			bTreeFile.writeBytable[(Byte, Long, List[Long])](
				(
					BTree.FREE_LIST_MARKER,
					freeListHead.getOrElse(0L),
					List[Long]()
				)
			)(
				Bytable.tuple3Bytable(
					Bytable.byteBytable,
					Bytable.longBytable,
					new Bytable[List[Long]](
						size,
						_ => ???,
						(_, bb) => 0.until(size).foreach(_ => bb.put(0.toByte))
					)
				)
			)
		else
			bTreeFile.writeBytable[(Byte, Long)](
				(
					BTree.FREE_LIST_MARKER,
					freeListHead.getOrElse(0L)
				)
			)(
				Bytable.tuple2Bytable(
					Bytable.byteBytable,
					Bytable.longBytable
				)
			)
		
		setFreeList(node.pos.some)
	}
	
}
