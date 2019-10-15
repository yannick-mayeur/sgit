package sgit
import sgit.fileIO.FileHelpers

case class Stage private (treeOpt: Option[Tree]) {
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

    val newTrees = blobs.flatMap { blob =>
      blob.name
        .split(FileHelpers.separator)
        // we drop 1 to remove the file from the path
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
          FileHelpers.stagePath(repository),
          newTree.hash
        )
        newTree.save(repository)
        this.copy(Some(newTree))
      case _ => this
    }
  }

  def getStagedFiles() = treeOpt.map(_.getAllBlobs()).map(_.map(_.name))

  def getContentFor(path: String) = treeOpt match {
    case Some(tree) =>
      tree.getBlobContentAt(path)
    case _ => None
  }

  def save(repository: Repository): Unit = treeOpt.foreach { tree =>
    FileHelpers.writeFile(
      FileHelpers.stagePath(repository),
      tree.hash
    )
  }

}

object Stage {
  def loadStage(repository: Repository) = {
    Stage(
      FileHelpers
        .getContent(FileHelpers.stagePath(repository))
        .flatMap(FileHelpers.getTree(repository, _))
        .map(Tree.fromXml(repository, _))
    )
  }
}
