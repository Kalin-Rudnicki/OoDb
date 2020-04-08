package oo_db.utils

import java.nio.ByteBuffer

case class Bytable[T](size: Int, fromBytes: ByteBuffer => T, toBytes: (T, ByteBuffer) => Unit)

object Bytable {
	
	// ====| Whole Numbers |=====
	
	implicit val byteBytable: Bytable[Byte] =
		Bytable[Byte](
			1,
			_.get,
			(t, bb) => bb.put(t)
		)
	
	implicit val shortBytable: Bytable[Short] =
		Bytable[Short](
			2,
			_.getShort,
			(t, bb) => bb.putShort(t)
		)
	
	implicit val intBytable: Bytable[Int] =
		Bytable[Int](
			4,
			_.getInt,
			(t, bb) => bb.putInt(t)
		)
	
	implicit val longBytable: Bytable[Long] =
		Bytable[Long](
			8,
			_.getLong,
			(t, bb) => bb.putLong(t)
		)
	
	
	// =====| Floating Point Numbers |=====
	
	implicit val floatBytable: Bytable[Float] =
		Bytable[Float](
			4,
			_.getFloat,
			(t, bb) => bb.putFloat(t)
		)
	
	implicit val doubleBytable: Bytable[Double] =
		Bytable[Double](
			8,
			_.getDouble,
			(t, bb) => bb.putDouble(t)
		)
	
	
	// =====| Tuples |=====
	
	def tuple2Bytable[T1, T2](b1: Bytable[T1], b2: Bytable[T2]): Bytable[(T1, T2)] =
		Bytable[(T1, T2)](
			b1.size + b2.size,
			bb => {
				val v2: T2 = b2.fromBytes(bb)
				val v1: T1 = b1.fromBytes(bb)
				(v1, v2)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
			}
		)
	
}
