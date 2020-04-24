package spyre.utils

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer

import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

// TODO : Experiment with Direct/NonDirect buffer
class BytableRAF(file: File, mode: String) extends RandomAccessFile(file, mode) {

	// Utils

	def readMaybeLong: Option[Long] =
		for (
			r <- readLong.some;
			iN <-
				if (r == 0)
					None
				else
					r.some
		) yield iN

	// Read
	
	def readBytable[T](implicit b: Bytable[T]): T = {
		val array: Array[Byte] = Array.ofDim(b.size)
		readFully(array)
		b.fromBytes(ByteBuffer.wrap(array))
	}
	
	def readBytableList[T](n: Int)(implicit b: Bytable[T]): List[T] = {
		val array: Array[Byte] = Array.ofDim(n * b.size)
		readFully(array)
		val bb: ByteBuffer = ByteBuffer.wrap(array)
		
		var list: List[T] = Nil
		0.until(n).foreach(_ => {
			list = b.fromBytes(bb) :: list
		})
		list.reverse
	}
	
	def readBytableArr[T](n: Int)(implicit b: Bytable[T]): Array[T] = {
		val array: Array[Byte] = Array.ofDim(n * b.size)
		readFully(array)
		val bb: ByteBuffer = ByteBuffer.wrap(array)
		
		val array2: Array[T] = Array.ofDim(n).asInstanceOf[Array[T]]
		@tailrec
		def loop(i: Int): Unit =
			if (i < n) {
				array2(i) = b.fromBytes(bb)
				loop(i + 1)
			}
		
		loop(0)
		array2
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
