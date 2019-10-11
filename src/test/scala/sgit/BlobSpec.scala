package sgit

import org.scalatest._

class BlobSpec extends FlatSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared tear down
    }
  }

  "Blob" should "calculate hash based on content" in {
    val blob1 = Blob("foo", "foobarbaz")
    val blob2 = Blob("bar", "foobarbaz")
    assert(blob1.hash == blob2.hash)
  }
}
