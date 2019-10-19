package sgit

import org.scalatest._
import java.io.File
import scala.language.postfixOps

class RepositrySpec extends FlatSpec with Matchers {
  var currentDirPath: String = ""
  override def withFixture(test: NoArgTest) = {
    currentDirPath = System.getProperty("user.dir")
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
    val repo = Repository.initRepository(currentDirPath)
    assert(repo nonEmpty)
    assert(new File(".sgit").exists())
    assert(new File(s".sgit${File.separator}HEAD").exists())
    assert(new File(s".sgit${File.separator}STAGE").exists())
    assert(new File(s".sgit${File.separator}tags").exists())
    assert(new File(s".sgit${File.separator}trees").exists())
    assert(new File(s".sgit${File.separator}blobs").exists())
    assert(new File(s".sgit${File.separator}branches").exists())
  }

  it should "check if .sgit exists in same directory" in {
    Repository.initRepository(currentDirPath)
    assert(Repository.getRepository(currentDirPath).nonEmpty)
  }

  it should "check if .sgit exists in parent directory" in {
    pending
  }

  it should "give path in repo for file" in {
    val repo = Repository.initRepository(currentDirPath)
    val file = new File("foobarbaz")
    file.createNewFile()
    val path = repo.get.getPathInRepositoryFor(file.getCanonicalPath)
    path shouldBe "/foobarbaz"
    file.delete()
  }
}
