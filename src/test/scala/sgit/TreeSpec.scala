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
    val tree1 =
      Tree("azer", Seq(Tree("test", Seq(), Seq(Blob("foo", "bar")))), Seq())
    val tree2 =
      Tree("azer", Seq(Tree("test", Seq(), Seq(Blob("foo", "bar")))), Seq())
    assert(tree1.hash == tree2.hash)
  }
}
