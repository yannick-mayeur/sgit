package sgit

case class Branch(
    name: String,
    head: Commit
) extends Reference {
  def verboseToString() = {
    s"$name ${head.hash}  ${head.message}"
  }

  def getName() = {
    name
  }

  def save(writeBranchToRepository: Option[String] => (String => Unit)) = {
    writeBranchToRepository(Some(name))(head.hash)
  }
}

object Branch {
  def load(
      getBranchContentFrom: String => Option[String],
      getCommitXmlFrom: String => Option[xml.Node],
      getTreeXmlFrom: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String],
      name: String
  ): Option[Branch] = {
    getBranchContentFrom(name)
      .flatMap(getCommitXmlFrom(_))
      .flatMap(
        Commit.fromXml(getCommitXmlFrom, getTreeXmlFrom, getBlobContentFrom, _)
      )
      .map(Branch(name, _))
  }
}
