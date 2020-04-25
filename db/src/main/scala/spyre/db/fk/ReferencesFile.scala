package spyre.db.fk

import java.io.{File, RandomAccessFile}

class ReferencesFile(private val io: IoManager) {

}

object ReferencesFile {

  // Constants
  val MAGIC_NUMBER: Int = 9691574

  val MIN_SIZE_INCREMENT = 3

  // Getting a ReferencesFile instance

  def create(path: String, sizeIncrement: Int): ReferencesFile = {
    if (sizeIncrement <= MIN_SIZE_INCREMENT)
      throw new IllegalArgumentException(s"sizeIncrement must be >= $MIN_SIZE_INCREMENT")

    val file = new File(path)
    if (file.exists)
      throw new IllegalArgumentException(s"File already exists at path $path")

    file.getParentFile.mkdirs

    val fkFile = new RandomAccessFile(file, "rw")
    fkFile.writeInt(MAGIC_NUMBER)
    fkFile.writeInt(sizeIncrement)
    fkFile.writeLong(0L)
    fkFile.close

    new ReferencesFile(new IoManager(path))
  }

  def load(path: String): ReferencesFile = {
    val file = new File(path)
    if (!file.exists)
      throw new IllegalArgumentException(s"File does not exist at path $path")

    new ReferencesFile(new IoManager(path))
  }

}
