package sgit
import scala.annotation.tailrec

object Merge {
  def getAlignedSubSequence(
      commonAncestor: String,
      change1: String,
      change2: String
  ) = {
    val newIndex = (x: Int) => math.max(1, x - 1)

    @tailrec
    def getDiff(
        l1: List[String],
        l2: List[String],
        l3: List[String],
        i: Int,
        j: Int,
        k: Int,
        diff: List[Option[String]]
    ): List[Option[String]] = {
      if (i > 0 && j > 0 && k > 0 && l1(i - 1) == l2(j - 1) && l2(j - 1) == l3(
            k - 1
          )) {
        getDiff(
          l1,
          l2,
          l3,
          i - 1,
          j - 1,
          k - 1,
          diff :+ Some(l1(i - 1))
        )
      } else if (i == 1 && j == 1 && k == 1)
        diff :+ None
      else if (i > 0 && j > 0 && k > 0) {
        getDiff(
          l1,
          l2,
          l3,
          newIndex(i),
          newIndex(j),
          newIndex(k),
          diff :+ None
        )
      } else {
        val maxLength = List(l1.size, l2.size, l3.size).reduce(_ max _)
        diff ++ List.fill(maxLength - diff.size)(None)
      }
    }
    val splitByLine = (text: String) => text.split("\n").toList

    val lines1 = splitByLine(commonAncestor)
    val lines2 = splitByLine(change1)
    val lines3 = splitByLine(change2)
    val result = getDiff(
      lines1,
      lines2,
      lines3,
      lines1.size,
      lines2.size,
      lines3.size,
      List.empty[Option[String]]
    ).reverse
    if (result.isEmpty) List()
    else result
  }

  def align(
      alignedSubSequence: List[Option[String]],
      sequenceToAlign: List[String],
      acc: List[Option[String]]
  ): List[Option[String]] = {
    (alignedSubSequence, sequenceToAlign) match {
      case ((x :: xs), (y :: ys)) if x == Some(y) =>
        align(xs, ys, acc :+ Some(y))
      case ((None :: xs), (y :: ys))
          if ((None :: xs).size) < ((y :: ys).size + 1) =>
        align(xs, ys, acc :+ Some(y))
      case ((None :: xs), (y :: ys))
          if xs
            .collectFirst { case Some(x) => Some(x) }
            .contains(Some(y)) =>
        align(xs, y :: ys, acc :+ None)
      case ((None :: xs), (y :: ys)) => align(xs, ys, acc :+ Some(y))
      case (Nil, Nil)                => acc
    }
  }
}
