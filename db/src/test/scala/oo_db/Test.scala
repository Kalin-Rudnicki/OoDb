package oo_db

import java.io.File

import cli.ScallopUtils._
import scalaz.std.option.optionSyntax._
import oo_db.db.BTree

import scala.collection.mutable.{Map => MMap}
import scala.util.Random

object Test {
	
	class Conf(arguments: Seq[String]) extends MainConfReqSub(arguments) {
		addSubcommand(
			new SubConfReqSub("speeds") {
				addSubcommand(
					new SubConf("INSERT") {
						// TODO
						override protected def onExec(): Unit = ???
					}
				)
			}
		)
		addSubcommand(
			new SubConf("test") {
				val testSize = opt[Int](name = "test-size", required = true, short = 's')
				val order = opt[Int](name = "order", required = true)
				val path = opt[String](name = "path", default = "res/test.bTree".some)
				val increment = opt[Int](name = "increment", default = 10.some)
				
				validate(testSize)(s => {
					if (s > 0)
						Right()
					else
						Left("test-size must be > 0")
				})
				validate(increment)(i => {
					if (i > 0)
						Right()
					else
						Left("increment must be > 0")
				})
				
				override protected def onExec(): Unit = {
					println
					println(s"Running test with size: ${testSize.toOption.get}")
					println
					
					val s = testSize.toOption.get
					val p = path.toOption.get
					val o = order.toOption.get
					val f = new File(p)
					val map: MMap[Long, Long] = MMap()
					
					val _inc = s / increment.toOption.get
					val inc =
						if (_inc == 0)
							1
						else
							_inc
					
					f.delete
					val bTree = BTree.create(o, p)
					
					try {
						
						// =====| Insert |=====
						println("=====| INSERT |=====")
						for (i <- 0.until(s)) {
							if (i > 0 && i % inc == 0)
								println(s"=====> $i")
							
							val k = Random.nextInt(s) + 1
							val v = Random.nextInt(s * 10) + 1
							
							map.put(k, v)
							bTree.insert(k, v)
							
							val act = bTree.get(k)
							val exp = v.some
							if (act != exp)
								throw new RuntimeException(s"Unexpected value for $k: Expected: $exp, Actual: $act")
							if (bTree.size != map.size)
								throw new RuntimeException(s"BTree has invalid size: ${bTree.size} != ${map.size}")
						}
						println(s"=====> $s")
						println
						
						if (bTree.size != map.size)
							throw new RuntimeException(s"BTree has invalid size: ${bTree.size} != ${map.size}")
						
						// Remoov
						println("=====| REMOVE |=====")
						var i = 0
						val pairs = Random.shuffle(map.toStream).toList
						val _inc2 = pairs.length / increment.toOption.get
						val inc2 =
							if (_inc2 == 0)
								1
							else
								_inc2
						for ((k, v) <- pairs) {
							if (i > 0 && i % inc2 == 0)
								println(s"=====> $i")
							i += 1
							
							val act = bTree.remove(k)
							val exp = map.remove(k)
							if (act != exp)
								throw new RuntimeException(s"Unexpected value for $k: Expected: $exp, Actual: $act")
							val act2 = bTree.get(k)
							if (act2.isDefined)
								throw new RuntimeException(s"Value was still found after deletion: $k => $act2")
							if (bTree.size != map.size)
								throw new RuntimeException(s"BTree has invalid size: ${bTree.size} != ${map.size}")
							
						}
						println(s"=====> ${pairs.length}")
						println
						
						if (bTree.size != 0)
							throw new RuntimeException(s"BTree was not empty in the end (${bTree.size})")
					}
					catch {
						case e: Exception =>
							// bTree.showStats
							println(s"size: ${bTree.size}")
							throw e
					}
					finally {
						bTree.close
						f.delete
					}
					
					println
				}
			}
		)
		addSubcommand(
			new SubConf("misc") {
				override protected def onExec(): Unit =
					Misc.test
			}
		)
	}
	
	def main(args: Array[String]): Unit = {
		val conf = new Conf(args)
		conf.exec
	}
	
}
