package spyre.db.fk

import java.io.File

import scala.annotation.tailrec

import scalaz.std.option.optionSyntax._
import scalaz.syntax.std.boolean._

import spyre.utils.{Bytable, BytableRAF}

class IoManager(path: String) {

  import IoManager._

  // =====| Constructor |=====

  private val fkFile: BytableRAF = new BytableRAF(new File(path), "rw")

  fkFile.seek(0)
  if (fkFile.readInt != ReferencesFile.MAGIC_NUMBER)
    throw new RuntimeException(s"'$path' is not a references file")

  private val sizeIncrement = fkFile.readInt
  if (sizeIncrement <= 0)
    throw new RuntimeException(
      s"Tried to load bad ReferencesFile (sizeIncrement = $sizeIncrement)"
    )

  private var freeList: Option[Long] = fkFile.readMaybeLong

  // =====| File Attributes |=====

  def setFreeListHead(pos: Option[Long]): Unit = {
    fkFile.seek(FREE_LIST_POSITION)
    freeList = pos
    fkFile.writeLong(pos.getOrElse(0L))
  }

  private def allocate(total: Int): Long = {
    @tailrec
    def loop(last: Option[Long],
             pos: Option[Long]): (Option[Long], Option[Long], Option[Long]) =
      pos match {
        case None =>
          (None, None, None)
        case Some(p) =>
          fkFile.seek(p)
          val (available, next) = fkFile.readBytable(freeListHeadBytable)
          if (total == available)
            (last, p.some, next)
          else if (total < available) {
            val unusedStart: Long = p + 8 * (total + 1)
            val unusedTotal: Int = available - total - 1
            fkFile.writeBytable((unusedTotal, unusedStart.some))(
              freeListHeadBytable
            )
            (last, unusedStart.some, next)
          } else
            loop(p.some, next)
      }

    loop(None, freeList) match {
      case (_, None, _) =>
        fkFile.length
      case (None, Some(p), next) =>
        setFreeListHead(next)
        p
      case (Some(last), Some(p), next) =>
        fkFile.seek(last)
        fkFile.writeLong(next.getOrElse(0L))
        p
    }
  }

  // =====| Access |=====

  /**
    * TODO : Figure out how to structure list
    * : Set up necessary accesses
    * : Start work on ReferencesFile to provide a public api for this functionality
    */
  /**
    * > sizeIncrement must be at least 3 (the first "section" actually holds 1 less, so it can store the total and used
    * > the desired way to interface with this file is through the FkList
    * > You can write an initial value(s), and get back a pos
    * > You can read from a pos (which gives you a FkList)
    * > You can modify a FkList, and then re-write it (and it will return a pos if it needed to move itself)
    * > If a FkList is using (<= half - 1) of its total "blocks", it will add the rest of them to the free list
    */
  // =====| Private |=====

  private def rawWriteFkList(list: FkList): Unit = {
    val bytable = Bytable[FkList](
      8 * (list.vals.size + 1),
      ???, // Only used to write, no need to read
      (l, bb) => {
        bb.putInt(l.total)
        bb.putInt(l.vals.size)
        list.vals.foreach(bb.putLong)
      }
    )

    fkFile.seek(list.pos)
    fkFile.writeBytable(list)(bytable)
  }

  // =====| Public |=====

  def createAndWriteNewFkList(vals: List[Long]): FkList = {
    val total = ((vals.size + 1).toFloat / sizeIncrement).ceil.toInt * sizeIncrement - 1
    val list = FkList(allocate(total), total, vals)
    rawWriteFkList(list)
    list
  }

  def readFkList(pos: Long): FkList = {
    val (total, used) = fkFile.readBytable(fkListHeadBytable)
    val vals: List[Long] = fkFile.readBytableList(used)
    FkList(pos, total, vals)
  }

  def updateFkList(list: FkList): Option[FkList] =
    if (list.isEmpty_?) {
      free(list)
      None
    } else {
      val res: FkList =
        (list.ifTooBig(sizeIncrement), list.ifTooSmall(sizeIncrement)) match {
          case (Some(newList), _) =>
            free(list)
            newList.copy(pos = allocate(newList.total))
          case (None, Some((newList, toFree))) =>
            free(toFree)
            newList
          case (None, None) =>
            list
        }
      rawWriteFkList(res)
      res.some
    }

  def free(fkList: FkList): Unit = {
    fkFile.seek(fkList.pos)
    fkFile.writeBytable((fkList.total, freeList))(freeListHeadBytable)
    setFreeListHead(fkList.pos.some)
  }

}

object IoManager {

  // Constants
  val MAGIC_NUMBER_POSITION: Long = 0L
  val SIZE_INCREMENT_POSITION: Long = 4L
  val FREE_LIST_POSITION: Long = 8L

  // Bytable

  /**
    * (total, used)
    */
  val fkListHeadBytable: Bytable[(Int, Int)] =
    Bytable.tuple2Bytable(Bytable.intBytable, Bytable.intBytable)

  /**
    * (total, next)
    */
  val freeListHeadBytable: Bytable[(Int, Option[Long])] =
    Bytable(
      12,
      bb => (bb.getInt, BytableRAF.convertToMaybeLong(bb.getLong)),
      (t, bb) => {
        bb.putInt(t._1)
        bb.putLong(t._2.getOrElse(0L))
      }
    )

  // Types

  case class FkList(pos: Long, total: Int, vals: List[Long]) {

    def add(vals: List[Long]): FkList =
      FkList(pos, total, vals ::: this.vals)

    def remove(vals: List[Long]): FkList = {
      val set = vals.toSet
      FkList(pos, total, this.vals.filter(v => !set.contains(v)))
    }

    def isEmpty_? : Boolean = vals.isEmpty

    def ifTooSmall(sizeIncrement: Int): Option[(FkList, FkList)] = {
      val usedBlocks = (vals.size + 1) / sizeIncrement
      val totalBlocks = (total + 1) / sizeIncrement
      (usedBlocks <= totalBlocks / 2 - 1).option(
        (
          FkList(pos, usedBlocks * sizeIncrement - 1, vals),
          FkList(
            pos + usedBlocks * sizeIncrement * 8,
            (totalBlocks - usedBlocks) * sizeIncrement - 1,
            Nil
          )
        )
      )
    }

    def ifTooBig(sizeIncrement: Int): Option[FkList] =
      if (vals.size > total)
        FkList(0, total + sizeIncrement, vals).some
      else
        None

  }

}
