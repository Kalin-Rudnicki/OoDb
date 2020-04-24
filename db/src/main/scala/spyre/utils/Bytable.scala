package spyre.utils

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
	
	def tuple2Bytable[T1, T2](
								 b1: Bytable[T1],
								 b2: Bytable[T2],
							 ): Bytable[(T1, T2)] =
		Bytable[(T1, T2)](
			b1.size + b2.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				(v1, v2)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
			}
		)
	
	def tuple3Bytable[T1, T2, T3](
									 b1: Bytable[T1],
									 b2: Bytable[T2],
									 b3: Bytable[T3],
								 ): Bytable[(T1, T2, T3)] =
		Bytable[(T1, T2, T3)](
			b1.size + b2.size + b3.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				(v1, v2, v3)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
			}
		)
	
	def tuple4Bytable[T1, T2, T3, T4](
										 b1: Bytable[T1],
										 b2: Bytable[T2],
										 b3: Bytable[T3],
										 b4: Bytable[T4],
									 ): Bytable[(T1, T2, T3, T4)] =
		Bytable[(T1, T2, T3, T4)](
			b1.size + b2.size + b3.size + b4.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				(v1, v2, v3, v4)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
			}
		)
	
	def tuple5Bytable[T1, T2, T3, T4, T5](
											 b1: Bytable[T1],
											 b2: Bytable[T2],
											 b3: Bytable[T3],
											 b4: Bytable[T4],
											 b5: Bytable[T5],
										 ): Bytable[(T1, T2, T3, T4, T5)] =
		Bytable[(T1, T2, T3, T4, T5)](
			b1.size + b2.size + b3.size + b4.size + b5.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				(v1, v2, v3, v4, v5)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
			}
		)
	
	def tuple6Bytable[T1, T2, T3, T4, T5, T6](
												 b1: Bytable[T1],
												 b2: Bytable[T2],
												 b3: Bytable[T3],
												 b4: Bytable[T4],
												 b5: Bytable[T5],
												 b6: Bytable[T6],
											 ): Bytable[(T1, T2, T3, T4, T5, T6)] =
		Bytable[(T1, T2, T3, T4, T5, T6)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
			}
		)
	
	def tuple7Bytable[T1, T2, T3, T4, T5, T6, T7](
													 b1: Bytable[T1],
													 b2: Bytable[T2],
													 b3: Bytable[T3],
													 b4: Bytable[T4],
													 b5: Bytable[T5],
													 b6: Bytable[T6],
													 b7: Bytable[T7],
												 ): Bytable[(T1, T2, T3, T4, T5, T6, T7)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
			}
		)
	
	def tuple8Bytable[T1, T2, T3, T4, T5, T6, T7, T8](
														 b1: Bytable[T1],
														 b2: Bytable[T2],
														 b3: Bytable[T3],
														 b4: Bytable[T4],
														 b5: Bytable[T5],
														 b6: Bytable[T6],
														 b7: Bytable[T7],
														 b8: Bytable[T8],
													 ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
			}
		)
	
	def tuple9Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9](
															 b1: Bytable[T1],
															 b2: Bytable[T2],
															 b3: Bytable[T3],
															 b4: Bytable[T4],
															 b5: Bytable[T5],
															 b6: Bytable[T6],
															 b7: Bytable[T7],
															 b8: Bytable[T8],
															 b9: Bytable[T9],
														 ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
			}
		)
	
	def tuple10Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
																   b1: Bytable[T1],
																   b2: Bytable[T2],
																   b3: Bytable[T3],
																   b4: Bytable[T4],
																   b5: Bytable[T5],
																   b6: Bytable[T6],
																   b7: Bytable[T7],
																   b8: Bytable[T8],
																   b9: Bytable[T9],
																   b10: Bytable[T10],
															   ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
			}
		)
	
	def tuple11Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
																		b1: Bytable[T1],
																		b2: Bytable[T2],
																		b3: Bytable[T3],
																		b4: Bytable[T4],
																		b5: Bytable[T5],
																		b6: Bytable[T6],
																		b7: Bytable[T7],
																		b8: Bytable[T8],
																		b9: Bytable[T9],
																		b10: Bytable[T10],
																		b11: Bytable[T11],
																	): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size + b11.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				val v11: T11 = b11.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
				b11.toBytes(t._11, bb)
			}
		)
	
	def tuple12Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
																			 b1: Bytable[T1],
																			 b2: Bytable[T2],
																			 b3: Bytable[T3],
																			 b4: Bytable[T4],
																			 b5: Bytable[T5],
																			 b6: Bytable[T6],
																			 b7: Bytable[T7],
																			 b8: Bytable[T8],
																			 b9: Bytable[T9],
																			 b10: Bytable[T10],
																			 b11: Bytable[T11],
																			 b12: Bytable[T12],
																		 ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size + b11.size + b12.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				val v11: T11 = b11.fromBytes(bb)
				val v12: T12 = b12.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
				b11.toBytes(t._11, bb)
				b12.toBytes(t._12, bb)
			}
		)
	
	def tuple13Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
																				  b1: Bytable[T1],
																				  b2: Bytable[T2],
																				  b3: Bytable[T3],
																				  b4: Bytable[T4],
																				  b5: Bytable[T5],
																				  b6: Bytable[T6],
																				  b7: Bytable[T7],
																				  b8: Bytable[T8],
																				  b9: Bytable[T9],
																				  b10: Bytable[T10],
																				  b11: Bytable[T11],
																				  b12: Bytable[T12],
																				  b13: Bytable[T13],
																			  ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size + b11.size + b12.size + b13.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				val v11: T11 = b11.fromBytes(bb)
				val v12: T12 = b12.fromBytes(bb)
				val v13: T13 = b13.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
				b11.toBytes(t._11, bb)
				b12.toBytes(t._12, bb)
				b13.toBytes(t._13, bb)
			}
		)
	
	def tuple14Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
																					   b1: Bytable[T1],
																					   b2: Bytable[T2],
																					   b3: Bytable[T3],
																					   b4: Bytable[T4],
																					   b5: Bytable[T5],
																					   b6: Bytable[T6],
																					   b7: Bytable[T7],
																					   b8: Bytable[T8],
																					   b9: Bytable[T9],
																					   b10: Bytable[T10],
																					   b11: Bytable[T11],
																					   b12: Bytable[T12],
																					   b13: Bytable[T13],
																					   b14: Bytable[T14],
																				   ): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size + b11.size + b12.size + b13.size + b14.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				val v11: T11 = b11.fromBytes(bb)
				val v12: T12 = b12.fromBytes(bb)
				val v13: T13 = b13.fromBytes(bb)
				val v14: T14 = b14.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
				b11.toBytes(t._11, bb)
				b12.toBytes(t._12, bb)
				b13.toBytes(t._13, bb)
				b14.toBytes(t._14, bb)
			}
		)
	
	def tuple15Bytable[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
																							b1: Bytable[T1],
																							b2: Bytable[T2],
																							b3: Bytable[T3],
																							b4: Bytable[T4],
																							b5: Bytable[T5],
																							b6: Bytable[T6],
																							b7: Bytable[T7],
																							b8: Bytable[T8],
																							b9: Bytable[T9],
																							b10: Bytable[T10],
																							b11: Bytable[T11],
																							b12: Bytable[T12],
																							b13: Bytable[T13],
																							b14: Bytable[T14],
																							b15: Bytable[T15],
																						): Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
		Bytable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)](
			b1.size + b2.size + b3.size + b4.size + b5.size + b6.size + b7.size + b8.size + b9.size + b10.size + b11.size + b12.size + b13.size + b14.size + b15.size,
			bb => {
				val v1: T1 = b1.fromBytes(bb)
				val v2: T2 = b2.fromBytes(bb)
				val v3: T3 = b3.fromBytes(bb)
				val v4: T4 = b4.fromBytes(bb)
				val v5: T5 = b5.fromBytes(bb)
				val v6: T6 = b6.fromBytes(bb)
				val v7: T7 = b7.fromBytes(bb)
				val v8: T8 = b8.fromBytes(bb)
				val v9: T9 = b9.fromBytes(bb)
				val v10: T10 = b10.fromBytes(bb)
				val v11: T11 = b11.fromBytes(bb)
				val v12: T12 = b12.fromBytes(bb)
				val v13: T13 = b13.fromBytes(bb)
				val v14: T14 = b14.fromBytes(bb)
				val v15: T15 = b15.fromBytes(bb)
				(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
			},
			(t, bb) => {
				b1.toBytes(t._1, bb)
				b2.toBytes(t._2, bb)
				b3.toBytes(t._3, bb)
				b4.toBytes(t._4, bb)
				b5.toBytes(t._5, bb)
				b6.toBytes(t._6, bb)
				b7.toBytes(t._7, bb)
				b8.toBytes(t._8, bb)
				b9.toBytes(t._9, bb)
				b10.toBytes(t._10, bb)
				b11.toBytes(t._11, bb)
				b12.toBytes(t._12, bb)
				b13.toBytes(t._13, bb)
				b14.toBytes(t._14, bb)
				b15.toBytes(t._15, bb)
			}
		)
	
}
