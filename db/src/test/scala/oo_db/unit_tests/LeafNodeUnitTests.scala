package oo_db.unit_tests

import oo_db.db.nodes.LeafNode
import org.scalatest.Matchers._
import org.scalatest.funspec.PathAnyFunSpec

class LeafNodeUnitTests extends PathAnyFunSpec {
	
	describe("LeafNode") {
		
		describe("Insertions") {
			
			describe("(size 3):") {
				
				describe("node 1") {
					
					val node = LeafNode(10, List(102), List(202), 30)
					
					describe("insert 1") {
						
						val afterInsert = node.insert(3, 20, 101, 201)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(101, 102), List(201, 202), 30)
						}
						
						it("should not have split") {
							assert(afterInsert.get._2.isEmpty)
						}
						
					}
					
					describe("insert 2") {
						
						val afterInsert = node.insert(3, 20, 102, 201)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(102), List(201), 30)
						}
						
						it("should not have split") {
							assert(afterInsert.get._2.isEmpty)
						}
						
					}
					
					describe("insert 3") {
						
						val afterInsert = node.insert(3, 20, 102, 202)
						
						it("should not be modified") {
							assert(afterInsert.isEmpty)
						}
						
					}
					
					describe("insert 4") {
						
						val afterInsert = node.insert(3, 20, 103, 203)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(102, 103), List(202, 203), 30)
						}
						
						it("should not have split") {
							assert(afterInsert.get._2.isEmpty)
						}
						
					}
					
				}
				
				describe("node 2") {
					
					val node = LeafNode(10, List(102, 104), List(202, 204), 30)
					
					describe("insert 1") {
						
						val afterInsert = node.insert(3, 20, 101, 201)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(101, 102), List(201, 202), 20)
						}
						
						it("should have split") {
							assert(afterInsert.get._2.isDefined)
						}
						
						it("split should equal") {
							afterInsert.get._2.get shouldEqual(104, LeafNode(20, List(104), List(204), 30))
						}
						
					}
					
					describe("insert 2") {
						
						val afterInsert = node.insert(3, 20, 102, 201)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(102, 104), List(201, 204), 30)
						}
						
						it("should not have split") {
							assert(afterInsert.get._2.isEmpty)
						}
						
					}
					
					describe("insert 3") {
						
						val afterInsert = node.insert(3, 20, 102, 202)
						
						it("should not be modified") {
							assert(afterInsert.isEmpty)
						}
						
					}
					
					describe("insert 4") {
						
						val afterInsert = node.insert(3, 20, 103, 203)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(102, 103), List(202, 203), 20)
						}
						
						it("should have split") {
							assert(afterInsert.get._2.isDefined)
						}
						
						it("split should equal") {
							afterInsert.get._2.get shouldEqual(104, LeafNode(20, List(104), List(204), 30))
						}
						
					}
					
					describe("insert 5") {
						
						val afterInsert = node.insert(3, 20, 105, 205)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(102, 104), List(202, 204), 20)
						}
						
						it("should have split") {
							assert(afterInsert.get._2.isDefined)
						}
						
						it("split should equal") {
							afterInsert.get._2.get shouldEqual(105, LeafNode(20, List(105), List(205), 30))
						}
						
					}
					
				}
				
				
			}
			
			describe("(size 4):") {
				
				describe("node 1") {
					
					val node = LeafNode(10, List(102, 104), List(202, 204), 30)
					
					describe("insert 1") {
						
						val afterInsert = node.insert(4, 20, 103, 203)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("should not have split") {
							assert(afterInsert.get._2.isEmpty)
						}
						
					}
					
				}
				
				describe("node 2") {
					
					val node = LeafNode(10, List(102, 104, 106), List(202, 204, 206), 30)
					
					describe("insert 1") {
						
						val afterInsert = node.insert(4, 20, 101, 201)
						
						it("should be modified") {
							assert(afterInsert.isDefined)
						}
						
						it("modified should equal") {
							afterInsert.get._1 shouldEqual LeafNode(10, List(101, 102), List(201, 202), 20)
						}
						
						it("should have split") {
							assert(afterInsert.get._2.isDefined)
						}
						
						it("split should equal") {
							afterInsert.get._2.get shouldEqual (104, LeafNode(20, List(104, 106), List(204, 206), 30))
						}
						
					}
					
				}
			
			}
			
		}
		
		describe("Removals") {
			
			describe("(size 3):") {
				
				describe("node 1") {
					
					val node = LeafNode(10, List(102, 104), List(202, 204), 30)
					
					describe("remove 1") {
						
						val afterRemove = node.remove(101)
						
						it("should not be modified") {
							assert(afterRemove.isEmpty)
						}
						
					}
					
					describe("remove 2") {
						
						val afterRemove = node.remove(102)
						
						it("should be modified") {
							assert(afterRemove.isDefined)
						}
						
						it("should equal") {
							afterRemove.get shouldEqual(202, LeafNode(10, List(104), List(204), 30), Some(Some(104)))
						}
						
					}
					
					describe("remove 3") {
						
						val afterRemove = node.remove(104)
						
						it("should be modified") {
							assert(afterRemove.isDefined)
						}
						
						it("should equal") {
							afterRemove.get shouldEqual(204, LeafNode(10, List(102), List(202), 30), None)
						}
						
					}
					
				}
				
				describe("node 2") {
					
					val node = LeafNode(10, List(102), List(202), 30)
					
					describe("remove 1") {
						
						val afterRemove = node.remove(103)
						
						it("should not be modified") {
							assert(afterRemove.isEmpty)
						}
						
					}
					
					describe("remove 2") {
						
						val afterRemove = node.remove(102)
						
						it("should be modified") {
							assert(afterRemove.isDefined)
						}
						
						it("should equal") {
							afterRemove.get shouldEqual (202, LeafNode(10, Nil, Nil, 30), Some(None))
						}
						
					}
					
				}
				
			}
			
		}
		
	}
	
}
