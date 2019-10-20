package sgit

case class Tag(
    name: String,
    commit: Commit
) extends Reference {
  def verboseToString() = {
    s"$name ${commit.hash}  ${commit.message}"
  }

  def getName() = {
    name
  }

  def save(writeTagToRepository: Option[String] => (String => Unit)) = {
    writeTagToRepository(Some(name))(commit.hash)
  }
}

object Tag {
  def load(
      getBranchContentFrom: String => Option[String],
      getCommitXmlFrom: String => Option[xml.Node],
      getTreeXmlFrom: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String],
      name: String
  ) = {
    getBranchContentFrom(name)
      .flatMap(getCommitXmlFrom(_))
      .flatMap(
        Commit.fromXml(getCommitXmlFrom, getTreeXmlFrom, getBlobContentFrom, _)
      )
      .map(Tag(name, _))
  }
}
