package spyre.unit_tests

import org.scalatest.Matchers._
import org.scalatest.funspec.PathAnyFunSpec
import spyre.db.b_tree.nodes.InternalNode

class InternalNodeUnitTests extends PathAnyFunSpec {

  describe("InternalNode") {

    describe("Insertions") {

      describe("(size 3):") {

        describe("node 1") {

          val node = new InternalNode(10, List(105), List(202, 206))

          describe("insert 1") {

            val afterInsert = node.insert(3, 20, 103, 204)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(103, 105),
                List(202, 204, 206)
              )
            }

            it("should not have split") {
              assert(afterInsert.get._2.isEmpty)
            }

          }

          describe("insert 2") {

            val afterInsert = node.insert(3, 20, 105, 204)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(105),
                List(202, 204)
              )
            }

            it("should not have split") {
              assert(afterInsert.get._2.isEmpty)
            }

          }

          describe("insert 3") {

            val afterInsert = node.insert(3, 20, 105, 206)

            it("should not be modified") {
              assert(afterInsert.isEmpty)
            }

          }

          describe("insert 4") {

            val afterInsert = node.insert(3, 20, 107, 208)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(105, 107),
                List(202, 206, 208)
              )
            }

            it("should not have split") {
              assert(afterInsert.get._2.isEmpty)
            }

          }

        }

        describe("node 2") {

          val node = new InternalNode(10, List(105, 109), List(202, 206, 210))

          describe("insert 1") {

            val afterInsert = node.insert(3, 20, 103, 204)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(103),
                List(202, 204)
              )
            }

            it("should have split") {
              assert(afterInsert.get._2.isDefined)
            }

            it("split should equal") {
              afterInsert.get._2.get shouldEqual (105, new InternalNode(
                20,
                List(109),
                List(206, 210)
              ))
            }

          }

          describe("insert 2") {

            val afterInsert = node.insert(3, 20, 107, 208)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(105),
                List(202, 206)
              )
            }

            it("should have split") {
              assert(afterInsert.get._2.isDefined)
            }

            it("split should equal") {
              afterInsert.get._2.get shouldEqual (107, new InternalNode(
                20,
                List(109),
                List(208, 210)
              ))
            }

          }

          describe("insert 3") {

            val afterInsert = node.insert(3, 20, 111, 212)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(105),
                List(202, 206)
              )
            }

            it("should have split") {
              assert(afterInsert.get._2.isDefined)
            }

            it("split should equal") {
              afterInsert.get._2.get shouldEqual (109, new InternalNode(
                20,
                List(111),
                List(210, 212)
              ))
            }

          }

        }

      }

      describe("(size 4):") {

        describe("node 1") {

          val node = new InternalNode(10, List(104, 106), List(203, 205, 207))

          describe("insert 1") {

            val afterInsert = node.insert(4, 20, 108, 209)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("should not have split") {
              assert(afterInsert.get._2.isEmpty)
            }

          }

        }

        describe("node 2") {

          val node =
            new InternalNode(10, List(104, 106, 108), List(203, 205, 207, 209))

          describe("insert 1") {

            val afterInsert = node.insert(4, 20, 110, 211)

            it("should be modified") {
              assert(afterInsert.isDefined)
            }

            it("modified should equal") {
              afterInsert.get._1 shouldEqual new InternalNode(
                10,
                List(104, 106),
                List(203, 205, 207)
              )
            }

            it("should have split") {
              assert(afterInsert.get._2.isDefined)
            }

            it("split should equal") {
              afterInsert.get._2.get shouldEqual (108, new InternalNode(
                20,
                List(110),
                List(209, 211)
              ))
            }

          }

        }

      }

    }

    describe("Removals") {}

  }

}
