package sgit

import org.scalatest._
import java.io.File
import scala.collection.mutable
import scala.util.Try

class StageSpec extends FlatSpec with Matchers {

  "Stage" should "add files to stage" in {
    var repositoryStage = ""
    val repositoryTrees = mutable.Map.empty[String, xml.Node]
    val repositoryBlobs = mutable.Map.empty[String, String]
    val workingDirectory = mutable.Map.empty[String, String]
    workingDirectory("/src/foo") = "bar"
    workingDirectory("ig") = "polytech"
    val initialStage = Stage(None)

    val writeStageToRepository = (content: String) => repositoryStage = content
    val writeTreeToRepository = (hash: Option[String]) =>
      (content: xml.Node) => 
        hash.foreach(repositoryTrees(_) = content)
    val writeBlobToRepository = (hash: Option[String]) =>
      (content: String) => hash.foreach(repositoryBlobs(_) = content)
    val getFileContentFrom =
      (name: String) => Try(workingDirectory(name)).toOption

    val newStage = initialStage.addFiles(
      writeStageToRepository,
      writeTreeToRepository,
      writeBlobToRepository,
      getFileContentFrom,
      Seq("/src/foo", "/bar")
    )
    println(repositoryStage)
  }
}
