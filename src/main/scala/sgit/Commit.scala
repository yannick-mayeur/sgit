package sgit

case class Commit(
    rootTree: Tree,
    timestamp: String,
    message: String,
    previous: Option[Commit]
) {
  val hash = Helper.getHashFor(this.toString)

  def toXml() = {
    <Commit>
      <message>{message}</message>
      <timestamp>{timestamp}</timestamp>
      <rootTreeHash>{rootTree.hash}</rootTreeHash>
      {
      previous match {
        case Some(commit) => <previousHash>{commit.hash}</previousHash>
        case _            => <previousHash></previousHash>
      }
    }
    </Commit>
  }

  def getLog(): String = {
    s"""
commit $hash
date: $timestamp
message: $message
${previous.map(commit => s"\n${commit.getLog()}").getOrElse("")}"""
  }

  def save(
      writeCommitToRepository: Option[String] => (xml.Node => Unit),
      writeTreeToRepository: Option[String] => (xml.Node => Unit),
      writeBlobToRepository: Option[String] => (String => Unit)
  ): Unit = {
    writeCommitToRepository(Some(hash))(toXml)
    rootTree.save(writeTreeToRepository, writeBlobToRepository)
  }

  def getContentFor(path: String) = rootTree.getBlobContentAt(path)

  def loadAllFiles(
      writeToWorkingDirectory: Option[String] => (String => Unit)
  ): Unit = {
    rootTree
      .getAllBlobs()
      .foreach(_.toWorkingDirectory(writeToWorkingDirectory))
  }
}

object Commit {
  def fromXml(
      getCommitXmlFrom: String => Option[xml.Node],
      getTreeXmlFrom: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String],
      node: xml.Node
  ): Option[Commit] = {
    val message = (node \ "message").text
    val timestamp = (node \ "timestamp").text
    val rootTreeHash = (node \ "rootTreeHash").text
    val previousHash = (node \ "previousHash").text
    val previous = getCommitXmlFrom(previousHash)
      .flatMap(
        Commit.fromXml(getCommitXmlFrom, getTreeXmlFrom, getBlobContentFrom, _)
      )
    getTreeXmlFrom(rootTreeHash)
      .map(Tree.fromXml(getTreeXmlFrom, getBlobContentFrom, _))
      .map(Commit(_, timestamp, message, previous))
  }
}
