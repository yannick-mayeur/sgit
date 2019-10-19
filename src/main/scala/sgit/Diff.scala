package sgit
import scala.annotation.tailrec
import scala.collection.mutable

case class Diff[T](changes: Seq[(String, T)])

object Diff {
  def getDiffBetweenStageAnd(
      getContentFor: String => Option[String],
      stage: Stage
  ) = {
    stage
      .getStagedFiles()
      .map(_.partition { path =>
        val stagedContentOpt = stage.getContentFor(path)
        val otherContentOpt = getContentFor(path)
        val diffs = for {
          stagedContent <- stagedContentOpt
          commitContent <- otherContentOpt
        } yield {
          val elem1 = stagedContent.split("\n")
          val elem2 = commitContent.split("\n")
          val d = Diff.getDiffBetweenElements(elem1, elem2)
          d
        }
        diffs
          .map(
            _.changes
              .map(change => change._1 == "> " || change._1 == "< ")
              .reduce(_ || _)
          )
          .getOrElse(true)
      }._1)
  }

  def isDiffWithWorkingDirecory(
      getContentInWD: String => Option[String],
      stage: Stage
  ) = {
    val getBlobContent = (path: String) =>
      Blob.loadFromWD(path, getContentInWD).map(_.content)
    getDiffBetweenStageAnd(getBlobContent, stage)
      .map(_.isEmpty)
      .getOrElse(true)
  }

  def isDiffWithLastCommit(head: Commit, stage: Stage): Boolean = {
    val getContentFor = (path: String) => head.rootTree.getBlobContentAt(path)
    getDiffBetweenStageAnd(getContentFor, stage)
      .map(_.isEmpty)
      .getOrElse(true)
  }

  def getDiffBetweenElements[T](elem1: Seq[T], elem2: Seq[T]) = {

    @tailrec
    def lcsLength[T](
        l1: Seq[T],
        l2: Seq[T],
        res: Seq[Seq[Int]]
    ): Seq[Seq[Int]] = {
      val index1 = res.size - 1
      if (l1.size <= index1) {
        res
      } else {
        val item1 = l1(index1)
        val newLine = l2.zipWithIndex.scanLeft(0) {
          case (previous, (item2, index2)) =>
            if (item1 == item2) {
              res(index1)(index2) + 1
            } else {
              Math.max(previous, res(index1)(index2 + 1))
            }
        }
        lcsLength(l1, l2, res ++ Seq(newLine))
      }
    }

    @tailrec
    def printDiff[T](
        matrix: Seq[Seq[Int]],
        l1: Seq[T],
        l2: Seq[T],
        i: Int,
        j: Int,
        diff: Diff[T]
    ): Diff[T] = {
      if (i > 0 && j > 0 && l1(i - 1) == l2(j - 1)) {
        printDiff(
          matrix,
          l1,
          l2,
          i - 1,
          j - 1,
          diff.copy(changes = diff.changes :+ ("  ", l1(i - 1)))
        )
      } else if (j > 0 && (i == 0 || matrix(i)(j - 1) >= matrix(i - 1)(j))) {
        printDiff(
          matrix,
          l1,
          l2,
          i,
          j - 1,
          diff.copy(changes = diff.changes :+ ("> ", l2(j - 1)))
        )
      } else if (i > 0 && (j == 0 || matrix(i)(j - 1) < matrix(i - 1)(j))) {
        printDiff(
          matrix,
          l1,
          l2,
          i - 1,
          j,
          diff.copy(changes = diff.changes :+ ("< ", l1(i - 1)))
        )
      } else {
        diff.copy(changes = diff.changes.reverse)
      }
    }

    printDiff(
      lcsLength(elem1, elem2, Seq.fill(1, elem2.size + 1)(0)),
      elem1,
      elem2,
      elem1.size,
      elem2.size,
      Diff(Seq())
    )
  }
}
