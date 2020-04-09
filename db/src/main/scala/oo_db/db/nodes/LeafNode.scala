package oo_db.db.nodes

import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

case class LeafNode(pos: Long, keys: List[Long], values: List[Long], nextLeaf: Long) extends Node {
	// println(s"# LeafNode($pos, $keys, $values, $nextLeaf)")
	
	override def size: Int =
		values.size + 1
	
	override def toList(order: Int): List[Long] =
		values.padTo(order - 1, 0L) ++ (nextLeaf :: keys).padTo(order, 0L)
	
	
	override def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])] = {
		afterInsert(key, value, keys, values, Nil, Nil) match {
			case None =>
				None
			case Some((newKeys, newValues)) =>
				if (newKeys.length < order)
					(
						new LeafNode(pos, newKeys, newValues, nextLeaf),
						None
					).some
				else {
					val (keys1, keys2) = NodeUtils.takeFirst((order + 1) / 2, newKeys)
					val (values1, values2) = NodeUtils.takeFirst((order + 1) / 2, newValues)
					val passBackVal = keys2.head
					(
						new LeafNode(pos, keys1, values1, freeListStart),
						(
							passBackVal,
							new LeafNode(freeListStart, keys2, values2, nextLeaf)
						).some
					).some
				}
		}
	}
	
	def find(key: Long): Option[Long] = {
		@tailrec
		def loop(k: List[Long], v: List[Long]): Option[Long] = ((k, v): @unchecked) match {
			case (Nil, Nil) =>
				None
			case (hK :: tK, hV :: tV) =>
				if (key == hK)
					hV.some
				else
					loop(tK, tV)
		}
		
		loop(keys, values)
	}
	
}

object LeafNode {
	
	def apply(pos: Long, order: Int, list: List[Long]): LeafNode = {
		val (values, nextLeaf :: keyStart) = NodeUtils.takeAtMax(order - 1, false, true, list, Nil)
		val (keys, _) = NodeUtils.takeAtMax(values.length, false, false, keyStart, Nil)
		new LeafNode(pos, keys, values, nextLeaf)
	}
	
}
