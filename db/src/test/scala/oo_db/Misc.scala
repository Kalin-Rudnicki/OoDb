package oo_db

import java.io.{File, IOException}

import oo_db.db.BTree
import scalaz.std.option.optionSyntax._

object Misc {
	
	def test: Unit = {
	
		val path = "res/test-1.bTree"
		new File(path).delete()
		
		val bTree = BTree.create(3, path)
		try {
			
			for (i <- 1.to(5))
				bTree.insert(i, i * 10)
			
			bTree.showStats
			bTree.remove(5)
			
			bTree.showStats
			bTree.remove(4)
			
			bTree.showStats
			bTree.remove(3)
			
			bTree.showStats
			bTree.remove(2)
			
			bTree.showStats
			bTree.remove(1)
			
		} finally {
			bTree.showStats
			
			bTree.close
			new File(path).delete()
		}
		
	}
	
}
