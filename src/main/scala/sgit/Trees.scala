package sgit
import sgit.fileIO.FileHelpers

case class Tree(
    name: String,
    trees: Seq[Tree],
    blobs: Seq[Blob]
) {
  val hash = FileStatus.getHashFor(this.toString())
  val toXml = () => {
    <Tree>
      <name>{name}</name>
      <trees>
        {trees.map(tree => <tree name={tree.name}>{tree.hash}</tree>)}
      </trees>
      <blobs>
        {blobs.map(blob => <blob name={blob.name}>{blob.hash}</blob>)}
      </blobs>
    </Tree>
  }
}

object Tree {
  def fromXml(repository: Repository, node: scala.xml.Node): Tree = {
    val name = (node \ "name").text
    val trees = (node \ "trees" \ "tree").flatMap { treeNode =>
      FileHelpers
        .getTree(repository, treeNode.text)
        .map(nextTreeNode => Tree.fromXml(repository, nextTreeNode))
    }
    val blobs = (node \ "blobs" \ "blob").map { node =>
      val content = FileHelpers.getBlob(repository, node.text)
      Blob(node \@ "name", content)
    }

    Tree(name, trees, blobs)
  }

  def save(repository: Repository, tree: Tree) {
    val treesPath = (repository: Repository, tree: Tree) =>
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}trees${FileHelpers.separator}${tree.hash}"
    FileHelpers.writeFile(
      treesPath(repository, tree),
      FileHelpers.formatXml(tree.toXml())
    )
    tree.trees.foreach(Tree.save(repository, _))
    tree.blobs.foreach(Blob.save(repository, _))
  }
}
