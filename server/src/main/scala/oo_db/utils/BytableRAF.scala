package oo_db.utils

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer

import scalaz.Functor

import scala.annotation.tailrec

// TODO : Experiment with Direct/NonDirect buffer
class BytableRAF(file: File, mode: String) extends RandomAccessFile(file, mode) {
	
	
	// Read
	
	def readBytable[T](implicit b: Bytable[T]): T = {
		val array: Array[Byte] = Array.ofDim(b.size)
		readFully(array)
		b.fromBytes(ByteBuffer.wrap(array))
	}
	
	def readBytable[T](n: Int)(implicit b: Bytable[T]): List[T] = {
		val array: Array[Byte] = Array.ofDim(n * b.size)
		readFully(array)
		val bb: ByteBuffer = ByteBuffer.wrap(array)
		
		var list: List[T] = Nil
		0.until(n).foreach(_ => {
			list = b.fromBytes(bb) :: list
		})
		list.reverse
	}
	
	
	// Write
	
	def writeBytable[T](t: T)(implicit b: Bytable[T]): Unit = {
		val bb: ByteBuffer = ByteBuffer.allocate(b.size)
		b.toBytes(t, bb)
		write(bb.array)
	}
	
	def writeBytable[T](tS: TraversableOnce[T])(implicit b: Bytable[T]): Unit = {
		val bb: ByteBuffer = ByteBuffer.allocate(b.size * tS.size)
		tS.foreach(t => b.toBytes(t, bb))
		write(bb.array)
	}

}
