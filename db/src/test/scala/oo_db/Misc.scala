package oo_db

import java.io.File

import oo_db.db.BTree
import scalaz.std.option.optionSyntax._

object Misc {
	
	def test: Unit = {
	
		val path = "res/test-1.bTree"
		new File(path).delete()
		
		val bTree = BTree.create(3, path)
		
		for (i <- 1.to(5))
			bTree.insert(i, i * 10)
		
		bTree.showStats
		
		bTree.remove(5)
		
		bTree.close
		
	}
	
}
