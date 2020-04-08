package oo_db

import java.io.RandomAccessFile

object Test2 {
	
	def main(args: Array[String]): Unit = {
		val path = "res/ex.bin"
		val iF = new RandomAccessFile(path, "rw")
		iF.write(Array.ofDim[Byte](100).map(_ => 1.toByte))
		iF.close
		
		val i1 = 10L
		val i2 = 60L
		
		val f1 = new RandomAccessFile("res/test-1.bTree", "rw")
		val f2 = new RandomAccessFile("res/test-1.bTree", "rw")
		
		// YOU CAN HAVE MULTIPLE OPEN IN WRITE MODE!!!!!
		
		println
		f1.seek(i1)
		f2.seek(i2)
		println(f1.readByte)
		println(f2.readByte)
		
		f1.seek(i1)
		f2.seek(i2)
		f1.writeByte(5.toByte)
		f2.writeByte(25.toByte)
		
		println
		f1.seek(i2)
		f2.seek(i1)
		println(f1.readByte)
		println(f2.readByte)
		
		f1.seek(i2)
		f2.seek(i1)
		f1.writeByte(15.toByte)
		f2.writeByte(35.toByte)
		
		println
		f1.seek(i1)
		f2.seek(i1)
		println(f1.readByte)
		println(f2.readByte)
		
		println
		f1.seek(i2)
		f2.seek(i2)
		println(f1.readByte)
		println(f2.readByte)
		
	}
	
}
