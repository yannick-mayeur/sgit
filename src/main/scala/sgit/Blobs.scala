package sgit
import scala.xml._

case class Blob(
    name: String,
    content: String
) {
  val hash = FileStatus.getHashFor(content)
}

object Blob {}
