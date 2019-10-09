package sgit
import sgit.fileIO.FileHelpers

case class Stage private (treeOpt: Option[Tree]) {
  val stagePath = "${FileHelpers.separator}.sgit${FileHelpers.separator}STAGE"

  def addFiles(repository: Repository, canonicalPaths: Seq[String]) = {
    val blobs = canonicalPaths
      .map(canonicalPath => repository.getPathInRepositoryFor(canonicalPath))
      .map(path => (path, FileHelpers.getContent(path)))
      .flatMap {
        case (path, content) =>
          content.map(Blob(path, _))
      }
    val newTree = treeOpt match {
      case Some(tree) => Tree(tree.name, tree.trees, tree.blobs ++ blobs)
      case None       => Tree("", Seq(), blobs)
    }
    FileHelpers.writeFile(s"${repository.sgitFilePath}$stagePath", newTree.hash)
    Tree.save(repository, newTree)
    this.copy(Some(newTree))
  }
}

object Stage {
  def loadStage(repository: Repository) = {
    val stagePath =
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}STAGE"
    FileHelpers.getContent(stagePath) match {
      case Some(hash) if hash.trim.nonEmpty =>
        val tree =
          Tree.fromXml(repository, FileHelpers.getTree(repository, hash))
        Stage(Some(tree))
      case _ =>
        Stage(None)
    }
  }
}
