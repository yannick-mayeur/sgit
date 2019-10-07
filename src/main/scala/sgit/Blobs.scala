package sgit
import java.io.PrintWriter

case class Blob(
    hash: String,
    content: String
)

object Blob {
  val writeBlob = (blob: Blob, repository: Repository) => {
    val blobFile = new PrintWriter(
      s"${repository.sgitFile.getCanonicalPath()}/blobs/${blob.hash}"
    )
    blobFile.write(blob.content)
  }
}
