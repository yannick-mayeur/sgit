package sgit
import scala.xml._
import sgit.fileIO.FileHelpers

case class Head(category: String, ref: String) {
  def toXml() = <Head category={category}>{ref}</Head>

  def getCommit(repository: Repository) = category match {
    case "commit" =>
      FileHelpers
        .getCommit(repository, ref)
        .flatMap(Commit.fromXml(repository, _))
    case "branch" =>
      FileHelpers
        .getContent(
          s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}branches${FileHelpers.separator}$ref"
        )
        .flatMap { ref =>
          FileHelpers
            .getCommit(repository, ref)
            .flatMap(Commit.fromXml(repository, _))
        }

  }

  def update(hash: String, repository: Repository) = category match {
    case "commit" =>
      FileHelpers.writeFile(
        s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}HEAD",
        this.copy(ref = hash).toXml().toString
      )
    case "branch" =>
      FileHelpers.writeFile(
        s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}branches${FileHelpers.separator}$ref",
        hash
      )
  }
}

object Head {
  def fromXml(node: scala.xml.Node) = {
    val category = (node \@ "category")
    val ref = (node).text
    Head(category, ref)
  }

  def initialCommit(hash: String, repository: Repository): Unit = {
    FileHelpers.writeFile(
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}HEAD",
      Head("branch", "master").toXml.toString()
    )
    FileHelpers.writeFile(
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}branches${FileHelpers.separator}master",
      hash
    )
  }
}
