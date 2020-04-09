package oo_db

import java.io.{File, RandomAccessFile}

import oo_db.db.BTree
import oo_db.utils.{Bytable, BytableRAF}

import scala.util.Random

object Test {
	
	def testInsertionSpeed(path: String, order: Int, testSize: Int, printEvery: Int): Unit = {
		val file = new File(path)
		file.delete()
		
		val bTree = BTree.create(order, path)
		
		val startT = System.currentTimeMillis
		0.until(testSize).foreach(i => {
			if (i % printEvery == 0)
				println(s"#$i => ${System.currentTimeMillis - startT}ms")
			
			bTree.insert(Random.nextLong, 1L)
		})
		val endT = System.currentTimeMillis
		
		println
		println(s"  Total time: ${endT - startT}ms")
		println(s"Average time: ${(endT - startT).toDouble / testSize.toDouble}ms")
		
	}
	
	def testInsertionAndRetrievalSpeed(path: String, order: Int, testSize: Int, printEvery: Int): Unit = {
		val file = new File(path)
		file.delete()
		
		val bTree = BTree.create(order, path)
		
		val startT = System.currentTimeMillis
		0.until(testSize).foreach(i => {
			if (i % printEvery == 0)
				println(s"#$i => ${System.currentTimeMillis - startT}ms")
			
			bTree.insert(Random.nextLong, 1L)
		})
		val endT = System.currentTimeMillis
		
		println
		println(s"  Total time: ${endT - startT}ms")
		println(s"Average time: ${(endT - startT).toDouble / testSize.toDouble}ms")
		
		println
		val startT2 = System.currentTimeMillis
		0.until(testSize).foreach(i => {
			if (i % printEvery == 0)
				println(s"#$i => ${System.currentTimeMillis - startT2}ms")
			
			bTree.get(Random.nextLong)
		})
		val endT2 = System.currentTimeMillis
		
		println
		println(s"  Total time: ${endT2 - startT2}ms")
		println(s"Average time: ${(endT2 - startT2).toDouble / testSize.toDouble}ms")
	}
	
	def main(args: Array[String]): Unit = {
		
		testInsertionAndRetrievalSpeed("res/test-1.bTree", 50, 1000000, 10000)
	}
	
}
