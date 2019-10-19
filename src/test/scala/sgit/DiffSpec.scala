package sgit

import org.scalatest._
import scala.collection.mutable
import scala.util.Try

class DiffSpec extends FlatSpec with Matchers {
  var stage: Stage = Stage(None)
  var repositoryStage = ""
  val repositoryTrees = mutable.Map.empty[String, xml.Node]
  val repositoryBlobs = mutable.Map.empty[String, String]
  val workingDirectory = mutable.Map.empty[String, String]
  val writeStageToRepository = (content: String) => repositoryStage = content
  val writeTreeToRepository = (hash: Option[String]) =>
    (content: xml.Node) => hash.foreach(repositoryTrees(_) = content)
  val writeBlobToRepository = (hash: Option[String]) =>
    (content: String) => hash.foreach(repositoryBlobs(_) = content)
  val getFileContentFrom =
    (name: String) => Try(workingDirectory(name)).toOption

  override def withFixture(test: NoArgTest) = {
    // Shared setup (run at beginning of each test)
    workingDirectory("src/foo") = "bar"
    workingDirectory("ig") = "polytech"
    stage = stage.addFiles(
      writeStageToRepository,
      writeTreeToRepository,
      writeBlobToRepository,
      getFileContentFrom,
      List("/src/foo", "/ig")
    )
    try test()
    finally {
      // Shared tear down
    }
  }

  "The Diff object" should "two lists of elements" in {
    val list1 = "ABCDEFGH"
    val list2 = "BDEZGHB"
    val res = Diff.getDiffBetweenElements(list1.toList, list2.toList)
    res.changes shouldEqual List(
      ("< ", 'A'),
      ("  ", 'B'),
      ("< ", 'C'),
      ("  ", 'D'),
      ("  ", 'E'),
      ("< ", 'F'),
      ("> ", 'Z'),
      ("  ", 'G'),
      ("  ", 'H'),
      ("> ", 'B')
    )
  }

  it should "get diffs with content" in {
    workingDirectory("ig") = "poltech"
    val getContentFor =
      (name: String) => Try(workingDirectory(name.drop(1))).toOption
    val res = Diff.getDiffBetweenStageAnd(getContentFor, stage)
    res shouldEqual Some(List("/ig"))
  }

  it should "say if diff with working directory returns false" in {
    workingDirectory("ig") = "poltech"
    val getContentWD = (name: String) => Try(workingDirectory(name)).toOption
    Diff.isDiffWithWorkingDirecory(getContentWD, stage) should be(false)
  }

  it should "say if diff with working directory returns true" in {
    val getContentWD = (name: String) => Try(workingDirectory(name)).toOption
    Diff.isDiffWithWorkingDirecory(getContentWD, stage) should be(true)
  }
}
