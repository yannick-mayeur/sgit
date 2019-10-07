package sgit
import java.io.PrintWriter

case class Tree(
    hash: String,
    trees: Seq[Tree],
    blobs: Seq[Blob]
)

object Tree {
  def writeTree(tree: Tree, repository: Repository): Unit = {
    val treeFile = new PrintWriter(
      s"${repository.sgitFile.getCanonicalPath()}/trees/${tree.hash}"
    )
    tree.trees.foreach { tree =>
      treeFile.append(s"tree: $tree")
      Tree.writeTree(tree, repository)
    }
    tree.blobs.foreach { blob =>
      treeFile.append(s"blob: $blob")
      Blob.writeBlob(blob, repository)
    }
  }
}
