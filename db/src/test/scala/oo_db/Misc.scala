package oo_db

import java.io.File

import oo_db.db.b_tree.BTree

import scala.collection.mutable.{Map => MMap}
import scalaz.std.option.optionSyntax._

import scala.io.Source

object Misc {
	
	def test: Unit = {
		
		println
		println
		println("===============| Misc |====================")
		
		val path = "res/test-1.bTree"
		new File(path).delete()
		
		val ins = Source.fromFile("db/src/test/res/in.txt").getLines.next.split(",").map(i => i.toInt)
		val rem = Source.fromFile("db/src/test/res/out.txt").getLines.next.split(",").map(i => i.toInt)
		
		val map = MMap[Long, Long]()
		val bTree = BTree.create(5, path)
		
		try {
			
			ins.foreach(i => {
				map.put(i, i * 10)
				bTree.insert(i, i * 10)
			})
			
			var count = 0
			
			bTree.writeImage(s"res/$count")
			
			val remove = (r: Long) => {
				count += 1
				println(s"$count) Removing: $r")
				map.remove(r)
				bTree.remove(r)
				bTree.writeImage(s"res/$count", s"Removed $r".some)
				if (bTree.size != map.size)
					throw new RuntimeException(s"Size is wrong: ${bTree.size} != ${map.size}")
			}
			
			// Remove stuff
			
			rem.foreach(r => remove(r))
		} finally {
			/*
			println
			println("Final results:")
			bTree.showStats
			*/
			
			bTree.close
			new File(path).delete()
		}
		
	}
	
}
