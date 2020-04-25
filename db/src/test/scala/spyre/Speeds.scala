package spyre

import java.io.File

import scala.collection.mutable.{HashSet => HS}
import scala.util.Random

import spyre.db.b_tree.BTree

object Speeds {

  def testInsertionSpeed(path: String,
                         order: Int,
                         testSize: Int,
                         printEvery: Int): Unit = {
    val file = new File(path)
    file.delete()

    val bTree = BTree.create(order, path)

    val startT = System.currentTimeMillis
    0.until(testSize)
      .foreach(i => {
        if (i % printEvery == 0)
          println(s"#$i => ${System.currentTimeMillis - startT}ms")

        bTree.insert(Random.nextLong, 1L)
      })
    val endT = System.currentTimeMillis

    println
    println(s"  Total time: ${endT - startT}ms")
    println(s"Average time: ${(endT - startT).toDouble / testSize.toDouble}ms")

  }

  def testInsertionAndRetrievalSpeed(path: String,
                                     order: Int,
                                     testSize: Int,
                                     printEvery: Int): Unit = {
    val file = new File(path)
    file.delete()

    val bTree = BTree.create(order, path)

    val startT = System.currentTimeMillis
    0.until(testSize)
      .foreach(i => {
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
    0.until(testSize)
      .foreach(i => {
        if (i % printEvery == 0)
          println(s"#$i => ${System.currentTimeMillis - startT2}ms")

        bTree.get(Random.nextLong)
      })
    val endT2 = System.currentTimeMillis

    println
    println(s"  Total time: ${endT2 - startT2}ms")
    println(
      s"Average time: ${(endT2 - startT2).toDouble / testSize.toDouble}ms"
    )
  }

  def testInsertionRetrievalAndRemovalSpeed(path: String,
                                            order: Int,
                                            testSize: Int,
                                            printEvery: Int): Unit = {
    // Thread.dumpStack

    val file = new File(path)
    file.delete()

    val bTree = BTree.create(order, path)
    val set = HS[Long]()

    try {

      val startT = System.currentTimeMillis
      0.until(testSize)
        .foreach(i => {
          if (i % printEvery == 0)
            println(s"#$i => ${System.currentTimeMillis - startT}ms")

          val k = Random.nextLong
          set.add(k)
          bTree.insert(k, 1L)
        })
      val endT = System.currentTimeMillis

      println
      println(s"  Total time: ${endT - startT}ms")
      println(
        s"Average time: ${(endT - startT).toDouble / testSize.toDouble}ms"
      )

      println
      val startT2 = System.currentTimeMillis
      0.until(testSize)
        .foreach(i => {
          if (i % printEvery == 0)
            println(s"#$i => ${System.currentTimeMillis - startT2}ms")

          bTree.get(Random.nextLong)
        })
      val endT2 = System.currentTimeMillis

      println
      println(s"  Total time: ${endT2 - startT2}ms")
      println(
        s"Average time: ${(endT2 - startT2).toDouble / testSize.toDouble}ms"
      )

      println
      val shuffled = Random.shuffle(set.toList)
      val startT3 = System.currentTimeMillis
      var i = 0
      shuffled.foreach(k => {
        if (i % printEvery == 0)
          println(s"#$i => ${System.currentTimeMillis - startT2}ms")

        bTree.remove(k)
        i += 1
      })
      val endT3 = System.currentTimeMillis

      println
      println(s"  Total time: ${endT3 - startT3}ms")
      println(
        s"Average time: ${(endT3 - startT3).toDouble / testSize.toDouble}ms"
      )

      println
      println
      println("=====| Insert |=====")
      println(s"  Total time: ${endT - startT}ms")
      println(
        s"Average time: ${(endT - startT).toDouble / testSize.toDouble}ms"
      )
      println("=====| Get |=====")
      println(s"  Total time: ${endT2 - startT2}ms")
      println(
        s"Average time: ${(endT2 - startT2).toDouble / testSize.toDouble}ms"
      )
      println("=====| Remove |=====")
      println(s"  Total time: ${endT3 - startT3}ms")
      println(
        s"Average time: ${(endT3 - startT3).toDouble / testSize.toDouble}ms"
      )

    } finally {
      bTree.close
    }
  }

}
