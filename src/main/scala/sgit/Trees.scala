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
  def fromXml(node: scala.xml.Node): Tree = {
    val name = (node \ "name").text
    println((node \ "trees" \ "tree").isEmpty)
    val trees = (node \ "trees" \ "tree").flatMap { treeNode =>
      FileHelpers
        .getTree(treeNode.text)
        .map(nextTreeNode => Tree.fromXml(nextTreeNode))
    }
    val blobs = (node \ "blobs" \ "blob").flatMap { node =>
      println(node.text)
      FileHelpers
        .getBlob(node.text)
        .map(content => Blob(node \@ "name", content))
    }

    Tree(name, trees, blobs)
  }
}
