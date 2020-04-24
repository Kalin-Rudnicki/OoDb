package spyre.db.fk

import java.io.File

import scalaz.std.option.optionSyntax._

import spyre.utils.BytableRAF

class IoManager(path: String) {

  // =====| Constructor |=====

  private val fkFile: BytableRAF = new BytableRAF(new File(path), "rw")

  fkFile.seek(0)
  if (fkFile.readInt != ReferencesFile.MAGIC_NUMBER)
    throw new RuntimeException(s"'$path' is not a references file")

  private var freeList: Option[Long] = fkFile.readMaybeLong

  // =====| ... |=====

  /**
    * TODO : Figure out how to structure list
    *      : Set up necessary accesses
    *      : Start work on ReferencesFile to provide a public api for this functionality
   */

}

object IoManager {

  // Constants
  val MAGIC_NUMBER_POSITION: Long = 0L
  val FREE_LIST_POSITION: Long = 4L

}
