package sgit
import scala.annotation.tailrec
import scala.collection.mutable

object Diff {
  def diffBetweenTexts(text1: String, text2: String) = {

    @tailrec
    def lcsLength[T](
        l1: List[T],
        l2: List[T],
        res: List[List[Int]]
    ): List[List[Int]] = {
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
        lcsLength(l1, l2, res ++ List(newLine))
      }
    }

    def printDiff[T](
        matrix: List[List[Int]],
        l1: List[T],
        l2: List[T],
        i: Int,
        j: Int
    ): Unit = {
      if (i > 0 && j > 0 && l1(i) == l2(j)) {
        printDiff(matrix, l1, l2, i - 1, j - 1)
        println(s" ${l1(i)}")
      } else if (j > 0 && (i == 0 || matrix(i)(j - 1) >= matrix(i - 1)(j))) {
        printDiff(matrix, l1, l2, i, j - 1)
        println(s"+ ${l2(i)}")
      } else if (i > 0 && (j == 0 || matrix(i)(j - 1) >= matrix(i - 1)(j))) {
        printDiff(matrix, l1, l2, i - 1, j)
        println(s"- ${l1(i)}")
      }
    }

    val lines1 = text1.toList
    val lines2 = text2.toList
    printDiff(
      lcsLength(lines1, lines2, List.fill(1, lines2.size + 1)(0)),
      lines1,
      lines2,
      lines1.size - 1,
      lines2.size - 1
    )
  }
}
