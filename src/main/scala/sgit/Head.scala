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
          FileHelpers.branchPath(repository, ref)
        )
        .flatMap { ref =>
          FileHelpers
            .getCommit(repository, ref)
            .flatMap(Commit.fromXml(repository, _))
        }

  }

  def update(hash: String, repository: Repository) = category match {
    case "commit" =>
      val newHead = this.copy(ref = hash)
      newHead.save(repository)
      newHead
    case "branch" =>
      Branch(ref, hash).save(repository)
      this
    case _ => this
  }

  def save(repository: Repository): Unit = {
    FileHelpers.writeFile(
      FileHelpers.headPath(repository),
      FileHelpers.formatXml(Head(category, ref).toXml())
    )
  }
}

object Head {
  def fromXml(node: scala.xml.Node) = {
    val category = (node \@ "category")
    val ref = (node).text
    Head(category, ref)
  }

  def initialCommit(hash: String, repository: Repository) = {
    val head = Head("branch", "master")
    head.save(repository)
    Branch("master", hash).save(repository)
    head
  }
}
