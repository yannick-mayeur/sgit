package sgit
import scala.xml._

case class Blob(
    name: String,
    content: String
) {
  val hash = Helper.getHashFor(content)

  def toWorkingDirectory(
      writeToWorkingDirectory: Option[String] => (String => Unit)
  ): Unit = {
    writeToWorkingDirectory(Some(Blob.formatForWD(name)))(content)
  }

  def save(writeToRepository: Option[String] => (String => Unit)): Unit = {
    writeToRepository(Some(hash))(content)
  }
}

object Blob {
  def formatForWD(path: String) = path.drop(1)

  def loadFromWD(path: String, getBlobContentFrom: String => Option[String]) = {
    getBlobContentFrom(formatForWD(path)).map(Blob(path, _))
  }

  def loadFromRepo(
      name: String,
      hash: String,
      getBlobContentFrom: String => Option[String]
  ) = {
    getBlobContentFrom(hash).map(Blob(name, _))
  }
}
