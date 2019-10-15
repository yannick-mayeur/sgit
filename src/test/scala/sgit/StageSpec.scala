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
    val stage = Stage(None)
    val foo = new File(s"$path${File.separator}foobarbaz")
    foo.createNewFile()
    val newStage =
      stage.addFiles(repository, Seq(s"$path${File.separator}foobarbaz"))
    assert(newStage.treeOpt.nonEmpty)
    val treeFile = new File(
      s"${repository.sgitFilePath}${File.separator}.sgit${File.separator}trees${File.separator}${newStage.treeOpt.get.hash}"
    )
    assert(treeFile.exists())
    foo.delete()
  }
}
