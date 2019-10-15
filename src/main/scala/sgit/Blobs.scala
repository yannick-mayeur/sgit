package sgit
import scala.xml._
import sgit.fileIO.FileHelpers

case class Blob(
    name: String,
    content: String
) {
  val hash = FileStatus.getHashFor(content)

  def toWorkingDirectory(repository: Repository): Unit = {
    FileHelpers.writeFile(
      s"${repository.sgitFilePath}$name",
      content
    )
  }

  def save(repository: Repository): Unit = {
    FileHelpers.writeFile(
      FileHelpers.blobPath(repository, hash),
      content
    )
  }
}

object Blob {}
