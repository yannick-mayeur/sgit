package sgit

import org.scalatest._

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
      FileStatus.getHashFor(
        "Tree(azer,List(Tree(test,List(),List(Blob(foo,bar)))),List())"
      )
    tree.hash shouldBe res
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
            Seq(Blob("baz", "foobar"))
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
            Seq(Blob("foo", "bar"), Blob("baz", "foobar"))
          )
        ),
        Seq()
      )
    merged shouldBe res
  }
}
