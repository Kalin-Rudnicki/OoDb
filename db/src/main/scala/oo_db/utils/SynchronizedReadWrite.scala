package oo_db.utils

import java.io.File
import java.io.Closeable
import java.util.concurrent.locks.ReentrantReadWriteLock

trait SynchronizedReadWrite[T <: Closeable] {
	
	// =====| Getting File |=====
	
	protected def getReadable: T
	
	protected def getWritable: T
	
	// =====| Handling Concurrency |=====
	
	// TODO : This is where "closed" files could be cached to decrease wait for opening a new file every time
	
	private val lock = new ReentrantReadWriteLock(true)
	
	def readFunction[I, O](f: (T, I) => O): I => O =
		i => {
			val file = getReadable
			lock.readLock.lock
			try {
				f(file, i)
			}
			finally {
				lock.readLock.unlock
				file.close
			}
		}
	
	def writeFunction[I, O](f: (T, I) => O): I => O =
		i => {
			val file = getWritable
			lock.writeLock.lock
			try {
				f(file, i)
			}
			finally {
				lock.writeLock.unlock
				file.close
			}
		}
	
}

class BytableReadWrite(private val file: File) extends SynchronizedReadWrite[BytableRAF] {
	
	override protected def getReadable: BytableRAF =
		new BytableRAF(file, "r")
	
	override protected def getWritable: BytableRAF =
		new BytableRAF(file, "rw")
	
}
