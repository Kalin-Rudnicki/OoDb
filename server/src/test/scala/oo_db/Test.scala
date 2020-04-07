package oo_db

import java.io.File

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
	
	def main(args: Array[String]): Unit = {
		
		testInsertionSpeed("res/test-1.bTree", 20, 10000, 100)
		
	}
	
}
