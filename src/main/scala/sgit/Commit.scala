package sgit
import sgit.fileIO.FileHelpers

case class Commit(
    rootTree: Tree,
    timestamp: String,
    message: String,
    previous: Option[Commit]
) {
  val hash = FileStatus.getHashFor(this.toString)

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

  def save(repository: Repository): Unit = {
    FileHelpers.writeFile(
      FileHelpers.commitPath(repository, hash),
      FileHelpers.formatXml(this.toXml())
    )
    repository.updateHead(hash)
    rootTree.save(repository)
  }

  def getContentFor(path: String) = rootTree.getBlobContentAt(path)

  def loadAllFiles(repository: Repository): Unit =
    rootTree.getAllBlobs().foreach(_.toWorkingDirectory(repository))
}

object Commit {
  def fromXml(repository: Repository, node: scala.xml.Node): Option[Commit] = {
    val message = (node \ "message").text
    val timestamp = (node \ "timestamp").text
    val rootTreeHash = (node \ "rootTreeHash").text
    val previousHash = (node \ "previousHash").text
    val previous = FileHelpers
      .getCommit(repository, previousHash)
      .flatMap(Commit.fromXml(repository, _))
    FileHelpers
      .getTree(repository, rootTreeHash)
      .map(Tree.fromXml(repository, _))
      .map(Commit(_, timestamp, message, previous))
  }
}
