package sgit
import sgit.fileIO.FileHelpers

case class Stage private (treeOpt: Option[Tree]) {
  val stagePath = s"${FileHelpers.separator}.sgit${FileHelpers.separator}STAGE"

  def addFiles(repository: Repository, canonicalPaths: Seq[String]) = {
    val treeReducer = (treeOpt1: Option[Tree], treeOpt2: Option[Tree]) => {
      (treeOpt1, treeOpt2) match {
        case (Some(tree1), Some(tree2)) => Some(tree2.merge(tree1))
        case (None, Some(tree2))        => Some(tree2)
        case (Some(tree1), None)        => Some(tree1)
        case _                          => None
      }
    }

    val blobs = canonicalPaths
      .map(canonicalPath => repository.getPathInRepositoryFor(canonicalPath))
      .map(path => (path, FileHelpers.getContent(path.drop(1))))
      .flatMap {
        case (path, content) =>
          content.map(Blob(path, _))
      }

    // we drop 1 to remove the file from the path
    val newTrees = blobs.flatMap { blob =>
      blob.name
        .split(FileHelpers.separator)
        .dropRight(1)
        .scanRight[Option[Tree]](None) {
          case (currentPath, previous) =>
            previous match {
              case Some(tree) =>
                Some(Tree(currentPath, Seq(tree), Seq()))
              case _ => Some(Tree(currentPath, Seq(), Seq(blob)))
            }
        }
        .headOption
    } :+ treeOpt

    newTrees.reduce(treeReducer) match {
      case Some(newTree) =>
        FileHelpers.writeFile(
          s"${repository.sgitFilePath}$stagePath",
          newTree.hash
        )
        Tree.save(repository, newTree)
        this.copy(Some(newTree))
      case _ => this
    }
  }

  def getStagedFiles() = treeOpt match {
    case Some(tree) =>
      Some(tree.blobs.map(_.name))
    case _ => None
  }

  def getContentFor(path: String) = treeOpt match {
    case Some(tree) =>
      tree.getBlobContentAt(path)
    case _ => None
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
