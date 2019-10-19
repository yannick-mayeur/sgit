package sgit

case class Stage private (treeOpt: Option[Tree]) {
  def addFiles(
      writeStageToRepository: String => Unit,
      writeTreeToRepository: Option[String] => (xml.Node => Unit),
      writeBlobToRepository: Option[String] => (String => Unit),
      getFileContentFrom: (String) => Option[String],
      pathFiles: Seq[String]
  ) = {
    val treeReducer = (treeOpt1: Option[Tree], treeOpt2: Option[Tree]) => {
      (treeOpt1, treeOpt2) match {
        case (Some(tree1), Some(tree2)) => Some(tree2.merge(tree1))
        case (None, Some(tree2))        => Some(tree2)
        case (Some(tree1), None)        => Some(tree1)
        case _                          => None
      }
    }

    val blobs = pathFiles.flatMap(Blob.loadFromWD(_, getFileContentFrom))

    val newTrees = blobs.flatMap { blob =>
      blob.name
        .split(Helper.separator)
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
        newTree.save(writeTreeToRepository, writeBlobToRepository)
        val newStage = this.copy(Some(newTree))
        newStage.save(writeStageToRepository)
        newStage
      case _ => this
    }
  }

  def getStagedFiles() = treeOpt.map(_.getAllBlobs()).map(_.map(_.name))

  def getContentFor(path: String) = treeOpt match {
    case Some(tree) =>
      tree.getBlobContentAt(path)
    case _ => None
  }

  def save(writeStageToRepository: String => Unit): Unit = treeOpt.foreach {
    tree =>
      writeStageToRepository(tree.hash)
  }

}

object Stage {
  def loadStage(
      getStageContent: () => Option[String],
      getTreeContentFrom: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String]
  ) = {
    Stage(
      getStageContent()
        .flatMap(getTreeContentFrom(_))
        .map(Tree.fromXml(getTreeContentFrom, getBlobContentFrom, _))
    )
  }
}
