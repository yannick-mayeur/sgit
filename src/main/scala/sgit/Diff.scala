package sgit
import scala.annotation.tailrec
import scala.collection.mutable

case class Change[T](changes: Seq[(String, T)])

case class Diff[T](fileName: String, changes: Change[T]) {
  def formatChanges(action: (Seq[(String, T)]) => String): String = {
    s"$fileName\n${action(changes.changes)}"
  }
}

object Diff {
  def getDiffBetweenStageAnd(
      getContentFor: String => Option[String],
      stage: Stage
  ) = {
    stage
      .getStagedFiles()
      .map(_.flatMap { path =>
        val stagedContentOpt = stage.getContentFor(path)
        val otherContentOpt = getContentFor(path)
        for {
          stagedContent <- stagedContentOpt
          commitContent <- otherContentOpt
        } yield {
          val elem1 = stagedContent.split("\n")
          val elem2 = commitContent.split("\n")
          val d = Diff(path, Diff.getChangesBetweenElements(elem1, elem2))
          d
        }
      })
  }

  def getDiffBetweenTrees(
      tree1: Tree,
      tree2: Tree
  ) = {
    tree1
      .getAllBlobs()
      .map { blob =>
        val content1 = blob.content.split("\n").toList
        val content2 =
          tree2.getBlobContentAt(blob.name).getOrElse("").split("\n").toList
        Diff(blob.name, Diff.getChangesBetweenElements(content1, content2))
      }
  }

  def getChangedFilesBetweenStageAnd(
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
          val d = Diff.getChangesBetweenElements(elem1, elem2)
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
    getChangedFilesBetweenStageAnd(getBlobContent, stage)
      .map(_.isEmpty)
      .getOrElse(true)
  }

  def isDiffWithLastCommit(head: Commit, stage: Stage): Boolean = {
    val getContentFor = (path: String) => head.rootTree.getBlobContentAt(path)
    getChangedFilesBetweenStageAnd(getContentFor, stage)
      .map(_.isEmpty)
      .getOrElse(true)
  }

  def getChangesBetweenElements[T](elem1: Seq[T], elem2: Seq[T]) = {

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
        diff: Change[T]
    ): Change[T] = {
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
      Change(Seq())
    )
  }
}
