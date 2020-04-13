package oo_db.db.b_tree

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font}
import java.io.File

import javax.imageio.ImageIO
import oo_db.db.b_tree.nodes._
import oo_db.utils.{Bytable, BytableRAF}
import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

class IoManager(path: String) {
	
	import IoManager._
	
	// =====| Constructor |=====
	
	private val bTreeFile: BytableRAF = new BytableRAF(new File(path), "rw")
	
	bTreeFile.seek(0)
	if (bTreeFile.readInt != BTree.MAGIC_NUMBER)
		throw new RuntimeException(s"'$path' is not a bTree")
	
	val order: Int = bTreeFile.readInt
	val minKeys: Int = (order.toFloat / 2f).ceil.toInt - 1
	
	if (order < BTree.MIN_ORDER)
		throw new RuntimeException(s"Something has gone seriously wrong... (order: $order)")
	
	private var height: Int = bTreeFile.readInt
	private var size: Int = bTreeFile.readInt
	
	private var root: Option[Long] =
		for (
			r <- bTreeFile.readLong.some;
			iN <-
				if (r == 0)
					None
				else
					r.some
		) yield iN
	private var freeListHead: Option[Long] =
		for (
			r <- bTreeFile.readLong.some;
			iN <-
				if (r == 0)
					None
				else
					r.some
		) yield iN
	
	def close: Unit =
		bTreeFile.close
	
	def showStats: Unit = {
		bTreeFile.seek(0L)
		val _magic = bTreeFile.readInt
		val _order = bTreeFile.readInt
		val _height = bTreeFile.readInt
		val _size = bTreeFile.readInt
		val _root = bTreeFile.readLong
		val _freeList = bTreeFile.readLong
		
		println
		println("=====| BTree Stats |=====")
		println(s"magicNumber: ${_magic}")
		println(s"      order: ${_order}, $order")
		println(s"     height: ${_height}, $height")
		println(s"       size: ${_size}, $size")
		println(s"       root: ${_root}, $root")
		println(s"   freeList: ${_freeList}, $freeListHead")
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
					println(s"$prefix => FREE_LIST(0x${values.head.toHexString})")
				case INTERNAL_NODE_MARKER =>
					println(s"$prefix => InternalNode($keys, $values)")
				case LEAF_NODE_MARKER =>
					println(s"$prefix => LeafNode($keys, ${values.reverse.tail.reverse}, ${values.last})")
			}
		}
		println("=====| EOF |=====")
	}
	
	def writeImage(path: String, message: Option[String] = None): Unit = {
		
		val stroke = 2
		val sqSize = 25
		val fontSize = 12
		val headerSize = 20
		val hSpace = 25
		val vSpace = 75
		
		@tailrec
		def calcWidth(depth: Int, pos: Long): Int =
			if (depth < height)
				calcWidth(depth + 1, readInternalNode(pos).children.head)
			else {
				var node = readLeafNode(pos).some
				var width = hSpace
				
				while (node.isDefined) {
					width += hSpace + node.get.keys.length * sqSize
					node =
						if (node.get.nextLeaf == 0L)
							None
					else
						readLeafNode(node.get.nextLeaf).some
				}
				
				width
			}
		
		
		val fontColor = Color.BLACK
		val leafColor = Color.GREEN
		val internalColor = Color.BLUE
		val lineColor = Color.RED
		
		val w = List(150, root.map(r => calcWidth(1, r)).getOrElse(hSpace)).max
		val h = height * sqSize + (height + 1) * vSpace
		
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
		val graphics = img.createGraphics
		graphics.setColor(Color.WHITE)
		graphics.fillRect(0, 0, w, h)
		
		graphics.setStroke(new BasicStroke(stroke))
		message match {
			case None =>
			case Some(msg) =>
				graphics.setFont(new Font("Monospaced", Font.PLAIN, headerSize))
				val fm2 = graphics.getFontMetrics
				val tH2 = ((vSpace - fm2.getHeight) / 2) + fm2.getAscent
				graphics.setColor(fontColor)
				graphics.drawString(
					msg,
					(w - fm2.stringWidth(msg)) / 2,
					tH2
				)
		}
		
		graphics.setFont(new Font("Monospaced", Font.PLAIN, fontSize))
		val fm = graphics.getFontMetrics
		
		var myStart = hSpace
		
		def loop(depth: Int, pos: Long): Int =
			if (depth < height) { // Internal Node
				val node = readInternalNode(pos)
				val children = node.children.map(c => {
					loop(depth + 1, c)
				})
				
				val h2 = (depth - 1) * (sqSize + vSpace) + vSpace
				val tH2 = h2 + ((sqSize - fm.getHeight) / 2) + fm.getAscent
				var me = children.sum / children.length - node.keys.length * sqSize / 2
				node.keys.zip(children).foreach {
					case (k, c) =>
						graphics.setColor(internalColor)
						graphics.drawRect(me, h2, sqSize, sqSize)
						graphics.setColor(fontColor)
						graphics.drawString(
							k.toString,
							me + (sqSize - fm.stringWidth(k.toString)) / 2,
							tH2
						)
						graphics.setColor(lineColor)
						graphics.drawLine(me, h2 + sqSize, c, h2 + sqSize + vSpace)
						
						me += sqSize
				}
				graphics.setColor(lineColor)
				graphics.drawLine(me, h2 + sqSize, children.last, h2 + sqSize + vSpace)
				
				me - node.keys.length * sqSize / 2
			}
			else { // Leaf Node
				val node = readLeafNode(pos)
				val h2 = (depth - 1) * (sqSize + vSpace) + vSpace
				val tH2 = h2 + ((sqSize - fm.getHeight) / 2) + fm.getAscent
				node.keys.foreach(k => {
					graphics.setColor(leafColor)
					graphics.drawRect(myStart, h2, sqSize, sqSize)
					graphics.setColor(fontColor)
					graphics.drawString(
						k.toString,
						myStart + (sqSize - fm.stringWidth(k.toString)) / 2,
						tH2
					)
					
					myStart += sqSize
				})
				
				val r = myStart - node.keys.length * sqSize / 2
				myStart += hSpace
				r
			}
		
		root.foreach(r => loop(1, r))
		
		val format = "png"
		ImageIO.write(img, "png", new File(s"$path.$format"))
	}
	
	// =====| File Attributes |=====
	
	// Read
	
	def nextFreePos: Long = {
		freeListHead.getOrElse(bTreeFile.length)
	}
	
	def getHeight: Int =
		height
	
	def getSize: Int =
		size
	
	def getRoot: Long =
		root.getOrElse(0L)
	
	// Write
	
	def setRoot(r: Option[Long]): Unit = {
		bTreeFile.seek(ROOT_POSITION)
		bTreeFile.writeLong(r.getOrElse(0L))
		root = r
	}
	
	private def setFreeList(f: Option[Long]): Unit = {
		bTreeFile.seek(FREE_LIST_POSITION)
		bTreeFile.writeLong(f.getOrElse(0L))
		freeListHead = f
	}
	
	def setHeight(h: Int): Unit = {
		bTreeFile.seek(HEIGHT_POSITION)
		bTreeFile.writeInt(h)
		height = h
	}
	
	def setSize(s: Int): Unit = {
		bTreeFile.seek(SIZE_POSITION)
		bTreeFile.writeInt(s)
		size = s
	}
	
	// =====| Nodes |=====
	
	// Read
	
	private def readData: List[Long] =
		bTreeFile.readBytable[Long](order * 2 - 1)
	
	
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
	
	def insertRootNode(node: Node[_]): Unit = {
		insertNewNode(node)
		setRoot(node.pos.some)
		setHeight(height + 1)
	}
	
	/**
	  * Will add a node at the next free point,
	  * if this nodes pos was not correctly set to that point,
	  * an error will be thrown
	  */
	def insertNewNode(node: Node[_]): Unit = {
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
	
	def writeNode(node: Node[_]): Unit = {
		bTreeFile.seek(node.pos)
		node match {
			case _: InternalNode => bTreeFile.writeByte(INTERNAL_NODE_MARKER)
			case _: LeafNode => bTreeFile.writeByte(LEAF_NODE_MARKER)
		}
		
		bTreeFile.writeBytable[Long](node.toList(order))
	}
	
	// NOT-SURE
	def deleteNode(node: Node[_], zeroOut: Boolean = false): Unit = {
		bTreeFile.seek(node.pos)
		val size = (order - 1) * 16
		if (zeroOut)
			bTreeFile.writeBytable[(Byte, Long, List[Long])](
				(
					FREE_LIST_MARKER,
					freeListHead.getOrElse(0L),
					List[Long]()
				)
			)(
				Bytable.tuple3Bytable(
					Bytable.byteBytable,
					Bytable.longBytable,
					new Bytable[List[Long]](
						size,
						_ => ???, // This is unimplemented on purpose
						(_, bb) => 0.until(size).foreach(_ => bb.put(0.toByte))
					)
				)
			)
		else
			bTreeFile.writeBytable[(Byte, Long)](
				(
					FREE_LIST_MARKER,
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

object IoManager {
	
	// Constants
	val MAGIC_NUMBER_POSITION: Long = 0L // Int
	val ORDER_POSITION: Long = 4L // Int
	val HEIGHT_POSITION: Long = 8L // Int
	val SIZE_POSITION: Long = 12L // Int
	val ROOT_POSITION: Long = 16L // Long
	val FREE_LIST_POSITION: Long = 24L // Long
	
	val FREE_LIST_MARKER: Byte = 0
	val INTERNAL_NODE_MARKER: Byte = 1
	val LEAF_NODE_MARKER: Byte = 2
	
}
