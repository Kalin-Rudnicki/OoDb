package oo_db

import java.io.File

import oo_db.OoDbTestDriver.Action._
import scalaz.std.option.optionSyntax._

import scala.collection.mutable.{Map => MMap}
import scala.util.Random

object OoDbTestDriver {
	
	def testInsertionsOnly(order: Int, path: String, testSize: Int, testAllEveryInsert: Boolean): Boolean = {
		var history: List[(Action, Long)] = Nil
		val map: MMap[Long, Long] = MMap()
		
		val maxNum = 10000000
		
		new File(path).delete()
		val bTree: BTree = BTree.create(order, path)
		
		def test(key: Long, expVal: Option[Long]): Unit = {
			val res: Option[Long] = bTree.get(key)
			if (res != expVal)
				throw InvalidRunException(s"~.get($key) => (act: $res) != (exp: $expVal)")
		}
		
		try {
			1.to(testSize).foreach(r => {
				/*
				val nextKey: Long = Random.nextLong()
				val nextValue: Long = Random.nextLong()
				*/
				val nextKey: Long = Random.nextInt(maxNum)
				val nextValue: Long = Random.nextInt(maxNum).some.map(i => {
					if (i == 0)
						1L
					else
						i
				}).getOrElse(1)
				
				
				val startT1 = System.currentTimeMillis
				map.put(nextKey, nextValue)
				val endT1 = System.currentTimeMillis
				history = (Insert, nextKey) :: history
				
				println(s"$r) INSERT: $nextKey -> $nextValue")
				val startT2 = System.currentTimeMillis
				bTree.insert(nextKey, nextValue)
				val endT2 = System.currentTimeMillis
				println(s"  map: ${endT1 - startT1}ms")
				println(s"bTree: ${endT2 - startT2}ms")
				
				if (testAllEveryInsert)
					map.foreach(t => {
						test(t._1, t._2.some)
					})
				else {
					test(nextKey, nextValue.some)
				}
				
			})
		}
		catch {
			case InvalidRunException(msg) =>
				bTree.showStats
				bTree.close
				new File(path).delete()
				println(s"=====> $msg")
				return false
			case e: RuntimeException =>
				bTree.showStats
				bTree.close
				new File(path).delete()
				throw e
		}
		
		true
	}
	
	sealed abstract class Action(val name: String)
	
	object Action {
		
		case object Insert extends Action("Insert")
		
		case object Delete extends Action("Insert")
		
	}
	
	case class History()
	
	case class InvalidRunException(msg: String) extends RuntimeException(msg)
	
}
