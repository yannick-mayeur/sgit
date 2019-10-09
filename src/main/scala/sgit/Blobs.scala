package sgit
import scala.xml._
import sgit.fileIO.FileHelpers

case class Blob(
    name: String,
    content: String
) {
  val hash = FileStatus.getHashFor(content)
}

object Blob {
  def save(repository: Repository, blob: Blob) {
    val blobsPath = (repository: Repository, blob: Blob) =>
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}blobs${FileHelpers.separator}${blob.hash}"
    FileHelpers.writeFile(
      blobsPath(repository, blob),
      blob.content
    )
  }
}
