package sgit

import org.scalatest._
import scala.collection.mutable

class TreeSpec extends FlatSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared tear down
    }
  }

  "Tree" should "calculate hash based toString" in {
    val tree =
      Tree("azer", Seq(Tree("test", Seq(), Seq(Blob("foo", "bar")))), Seq())
    val res =
      Helper.getHashFor(
        "Tree(azer,List(Tree(test,List(),List(Blob(foo,bar)))),List())"
      )
    tree.hash shouldBe res
  }

  it should "write to repository" in {
    var repository = mutable.Map.empty[String, String]
    def writeTreeToRepository =
      (name: Option[String]) =>
        (content: xml.Node) => name.foreach(repository(_) = content.toString)
    def writeBlobToRepository =
      (name: Option[String]) => (content: String) => ()
    val tree = Tree("foo", Seq(), Seq())
    tree.save(writeTreeToRepository, writeBlobToRepository)
    assert(repository.contains(tree.hash))
    repository(tree.hash) shouldEqual tree.toXml().toString()
  }

  it should "return content from blob at given path depth 0" in {
    val tree = Tree("", Seq(), Seq(Blob("/foo", "bar")))
    val res = tree.getBlobContentAt("/foo")
    res shouldEqual Some("bar")
  }

  it should "return content from blob at given path  depth 1" in {
    val tree =
      Tree("", Seq(Tree("src", Seq(), Seq(Blob("/src/foo", "bar")))), Seq())
    val res = tree.getBlobContentAt("/src/foo")
    res shouldEqual Some("bar")
  }

  it should "return content from blob at given path  depth 3" in {
    val tree = Tree(
      "",
      Seq(
        Tree(
          "src",
          Seq(
            Tree(
              "ig",
              Seq(
                Tree(
                  "polytech",
                  Seq(),
                  Seq(Blob("/src/ig/polytech/foo", "bar"))
                )
              ),
              Seq()
            )
          ),
          Seq()
        )
      ),
      Seq()
    )
    val res = tree.getBlobContentAt("/src/ig/polytech/foo")
    res shouldEqual Some("bar")
  }

  it should "merge into another tree" in {
    val tree1 =
      Tree("azer", Seq(Tree("test", Seq(), Seq(Blob("foo", "bar")))), Seq())
    val tree2 =
      Tree(
        "azer",
        Seq(
          Tree(
            "test",
            Seq(Tree("mitre", Seq(), Seq())),
            Seq(Blob("foo", "newbar"), Blob("baz", "foobar"))
          )
        ),
        Seq()
      )
    val merged = tree1.merge(tree2)
    val res =
      Tree(
        "azer",
        Seq(
          Tree(
            "test",
            Seq(Tree("mitre", Seq(), Seq())),
            Seq(Blob("foo", "newbar"), Blob("baz", "foobar"))
          )
        ),
        Seq()
      )
    merged shouldBe res
  }

  it should "return all blobs" in {
    val blob1 = Blob("foo", "bar")
    val blob2 = Blob("ig", "polytech")
    val tree = Tree("", Seq(Tree("src", Seq(), Seq(blob1))), Seq(blob2))
    val allBlobs = tree.getAllBlobs()
    allBlobs shouldBe blob1 :: blob2 :: Nil
  }
}
