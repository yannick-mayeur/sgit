package sgit
import scala.xml._

case class Head(category: String, ref: String) {
  def toXml() = <Head category={category}>{ref}</Head>

  def getCommit(
      getCommitXmlFrom: String => Option[xml.Node],
      getTreeXmlFrom: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String],
      getBranchContentFrom: String => Option[String]
  ) = category match {
    case "commit" =>
      getCommitXmlFrom(ref)
        .flatMap(
          Commit
            .fromXml(getCommitXmlFrom, getTreeXmlFrom, getBlobContentFrom, _)
        )
    case "branch" =>
      getBranchContentFrom(ref)
        .flatMap { ref =>
          getCommitXmlFrom(ref)
            .flatMap(
              Commit
                .fromXml(
                  getCommitXmlFrom,
                  getTreeXmlFrom,
                  getBlobContentFrom,
                  _
                )
            )
        }

  }

  def update(
      writeHeadToRepository: xml.Node => Unit,
      writeBranchToRepository: Option[String] => (String => Unit),
      hash: String
  ) = category match {
    case "commit" =>
      val newHead = this.copy(ref = hash)
      newHead.save(writeHeadToRepository)
      newHead
    case "branch" =>
      Branch(ref, hash).save(writeBranchToRepository)
      this
    case _ => this
  }

  def save(writeHeadToRepository: xml.Node => Unit): Unit = {
    writeHeadToRepository(Head(category, ref).toXml)
  }
}

object Head {
  def fromXml(node: scala.xml.Node) = {
    val category = (node \@ "category")
    val ref = (node).text
    Head(category, ref)
  }

  def initialCommit(
      writeHeadToRepository: xml.Node => Unit,
      writeBranchToRepository: Option[String] => (String => Unit),
      hash: String
  ) = {
    val head = Head("branch", "master")
    head.save(writeHeadToRepository)
    Branch("master", hash).save(writeBranchToRepository)
    head
  }
}
