package sgit

import org.scalatest._
import java.io.File

class StageSpec extends FlatSpec with Matchers {
  val path = System.getProperty("user.dir")
  val repository = Repository.initRepository(path).get
  override def withFixture(test: NoArgTest) = {
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      val sgit = new File(".sgit")
      if (sgit.exists()) {
        delete(new File(".sgit"))
      }
    }
  }

  def delete(file: File): Unit = {
    if (file.isDirectory()) {
      file.listFiles().foreach(delete(_))
    }
    file.delete()
  }

  "Stage" should "add given file" in {
    pending
  }
}
