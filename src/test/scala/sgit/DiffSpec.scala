package sgit

import org.scalatest._

class DiffSpec extends FlatSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared tear down
    }
  }
}
