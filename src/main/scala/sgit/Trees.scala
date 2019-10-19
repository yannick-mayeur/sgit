package sgit

case class Tree(
    name: String,
    trees: Seq[Tree],
    blobs: Seq[Blob]
) {
  val hash = Helper.getHashFor(this.toString())
  def toXml() = {
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

  def save(
      writeTreeToRepository: Option[String] => (xml.Node => Unit),
      writeBlobToRepository: Option[String] => (String => Unit)
  ): Unit = {
    writeTreeToRepository(Some(hash))(toXml())
    trees.foreach(_.save(writeTreeToRepository, writeBlobToRepository))
    blobs.foreach(_.save(writeBlobToRepository))
  }

  def getBlobContentAt(path: String): Option[String] = {
    path.split(Helper.separator).toList match {
      case x1 :: x2 :: Nil if x1 == name =>
        blobs
          .filter(_.name.split(Helper.separator).lastOption.contains(x2))
          .map(_.content)
          .headOption
      case x1 :: x2 :: xs if x1 == name =>
        trees
          .filter(_.name == x2)
          .headOption
          .flatMap(_.getBlobContentAt((x2 :: xs).mkString(Helper.separator)))
      case x :: Nil =>
        blobs
          .filter(_.name.split(Helper.separator).lastOption.contains(x))
          .map(_.content)
          .headOption
      case x => None
    }
  }

  def merge(newTree: Tree): Tree = newTree match {
    case Tree(name2, trees2, blobs2) if name == name2 =>
      val groupedTrees2 = trees2.groupBy(_.name)
      val newTrees = trees.filterNot { tree =>
        trees2.map(_.name).contains(tree.name)
      } ++ trees2.filterNot { tree2 =>
        trees.map(_.name).contains(tree2.name)
      } ++ trees
        .filter { tree =>
          trees2.map(_.name).contains(tree.name)
        }
        .flatMap(t => groupedTrees2(t.name).map(t.merge))
      val newBlobs = blobs.filterNot { blob =>
        blobs2.map(_.name).contains(blob.name)
      } ++ blobs2
      Tree(name, newTrees, newBlobs)
    case _ => this
  }

  def getAllBlobs(): Seq[Blob] = {
    trees.flatMap(_.getAllBlobs()) ++ blobs
  }
}

object Tree {
  def fromXml(
      getTreeFromRepository: String => Option[xml.Node],
      getBlobContentFrom: String => Option[String],
      node: scala.xml.Node
  ): Tree = {
    val name = (node \ "name").text
    val trees = (node \ "trees" \ "tree").flatMap { treeNode =>
      getTreeFromRepository(treeNode.text)
        .map(
          nextTreeNode =>
            Tree
              .fromXml(getTreeFromRepository, getBlobContentFrom, nextTreeNode)
        )
    }
    val blobs = (node \ "blobs" \ "blob").flatMap { node =>
      Blob.loadFromRepo(node \@ "name", node.text, getBlobContentFrom)
    }

    Tree(name, trees, blobs)
  }
}
