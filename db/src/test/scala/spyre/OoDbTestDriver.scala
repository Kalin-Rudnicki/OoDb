package spyre

import java.io.File

import scala.collection.mutable.{Map => MMap}
import scala.util.Random

import scalaz.std.option.optionSyntax._

import spyre.db.b_tree.BTree

object OoDbTestDriver {

  private val maxNum = 10000000

  def testInsertionsOnly(order: Int,
                         path: String,
                         testSize: Int,
                         testAllEveryInsert: Boolean): Boolean = {

    new File(path).delete()
    val bTree: BTree = BTree.create(order, path)
    val tester: TestState = new TestState(false, bTree)

    try {
      1.to(testSize)
        .foreach(r => {
          /*
				val nextKey: Long = Random.nextLong()
				val nextValue: Long = Random.nextLong()
				*/
          val nextKey: Long = Random.nextInt(maxNum)
          val nextValue: Long = Random
            .nextInt(maxNum)
            .some
            .map(i => {
              if (i == 0)
                1L
              else
                i
            })
            .getOrElse(1)

          tester.insert(nextKey, nextValue)
        })
    } catch {
      case InvalidTestException(msg) =>
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

  case class InvalidTestException(msg: String) extends RuntimeException(msg)

  class TestState(private val debug: Boolean, private val bTree: BTree) {
    val map: MMap[Long, Long] = MMap()

    def insert(key: Long, value: Long): Unit = {
      map.put(key, value)

      /*
			 * Should come out as 0 or 1 ms, anything more,
			 * and something really needs to be addressed
			 */
      if (debug) {
        println(s"INSERT: $key -> $value")
        val startT2 = System.currentTimeMillis
        bTree.insert(key, value)
        val endT2 = System.currentTimeMillis
        println(s"bTree: ${endT2 - startT2}ms")
      } else {
        bTree.insert(key, value)
      }
    }

    def remove(key: Long): Unit = ???

    def test(key: Long): Unit = ???

    def testAll: Unit = ???

  }

}
