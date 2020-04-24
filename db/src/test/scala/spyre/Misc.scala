package spyre

import java.io.File

import spyre.db.b_tree.BTree
import spyre.utils._

import scala.collection.mutable.{Map => MMap}
import scalaz.std.option.optionSyntax._

import scala.io.Source

import java.io.Closeable

object Misc {
	
	def spinUp(
				  run: Runnable*
			  ): List[Thread] = {
		run.map(r => {
			val t = new Thread(r)
			t.start
			t
		}).toList
	}
	
	def test1: Unit = {
		
		val f = new File("res/test.txt")
		val file = new BytableThreadLock(f)
		
		val read = file.readFunction[Int, Long]((f, i) => {
			println(s"READ: $i")
			Thread.sleep(1000)
			f.readLong
		})
		
		val write = file.writeFunction[(Int, Long), Unit]((f, i) => {
			println(s"WRITE: $i")
			Thread.sleep(1000)
			f.writeLong(1000)
		})
		
		try {
			write((0, 500))
			spinUp(
				() => write((1, 1000)),
				() => println(read(2)),
				() => println(read(4)),
				() => println(read(5)),
				() => println(read(6)),
				() => write((7, 2000)),
				() => println(read(8)),
				() => println(read(9)),
				() => println(read(10)),
				() => println(read(11)),
			).foreach(t => t.join)
		}
		finally {
			f.delete
		}
		
	}
	
	case class D(var i: Int)
	
	case class C(d: D) extends Closeable {
		override def close(): Unit = ()
	}
	
	class CF extends ThreadLock[C] {
		
		private val d = D(0)
		
		override protected def getReadable: C = C(d)
		
		override protected def getWritable: C = C(d)
		
		private val readF =
			readFunction[Int, Unit]((f, i) => {
				println(s"READ: $i")
				Thread.sleep(1000)
				println(s"$i => ${f.d.i}")
			})
		def read(i: Int) =
			readF(i)
		
		private val writeF =
			writeFunction[(Int, Int), Unit]((f, i) => {
				println(s"WRITE: ${i._1}")
				Thread.sleep(1000)
				f.d.i = i._2
			})
		def write(i: Int, v: Int) =
			writeF(i, v)
		
	}
	
	def test2: Unit = {
		
		val cf = new CF()
		
		spinUp(
			() => cf.write(1, 1000),
			() => cf.read(2),
			() => cf.read(4),
			() => cf.read(5),
			() => cf.read(6),
			() => cf.write(7, 2000),
			() => cf.read(8),
			() => cf.read(9),
			() => cf.read(10),
			() => cf.read(11),
		).foreach(t => t.join)
		
	}
	
	def test: Unit =
		test1
	
}
