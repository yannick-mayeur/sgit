package sgit

import org.scalatest._
import java.io.File

class RepositrySpec extends FlatSpec with Matchers {
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

  "Repository" should "create the .sgit structure" in {
    Repository.initRepository(".")
    assert(new File(".sgit").exists())
    assert(new File(s".sgit${File.separator}HEAD").exists())
    assert(new File(s".sgit${File.separator}STAGE").exists())
    assert(new File(s".sgit${File.separator}tags").exists())
    assert(new File(s".sgit${File.separator}trees").exists())
    assert(new File(s".sgit${File.separator}blobs").exists())
    assert(new File(s".sgit${File.separator}branches").exists())
  }

  it should "check if .sgit exists in same directory" in {
    Repository.initRepository(".")
    assert(Repository.isInRepository("."))
  }

  it should "check if .sgit exists in parent directory" in {
    Repository.initRepository("..")
    assert(Repository.isInRepository("."))
  }
}
