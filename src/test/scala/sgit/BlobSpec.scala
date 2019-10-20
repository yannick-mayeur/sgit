package sgit

import org.scalatest._
import scala.collection.mutable

class BlobSpec extends FlatSpec with Matchers {

  "Blob" should "calculate hash based on content" in {
    val blob1 = Blob("foo", "foobarbaz")
    val blob2 = Blob("bar", "foobarbaz")
    assert(blob1.hash == blob2.hash)
  }

  it should "write to working directory" in {
    var workingDirectory = mutable.Map.empty[String, String]
    def writeToWorkingDirectory =
      (name: Option[String]) =>
        (content: String) => name.foreach(workingDirectory(_) = content)
    val blob = Blob("/foo", "bar")
    blob.toWorkingDirectory(writeToWorkingDirectory)
    assert(workingDirectory.contains(blob.name.drop(1)))
    workingDirectory(blob.name.drop(1)) shouldEqual "bar"
  }

  it should "write to repository" in {
    var repository = mutable.Map.empty[String, String]
    def writeToRepository =
      (name: Option[String]) =>
        (content: String) => name.foreach(repository(_) = content)
    val blob = Blob("/foo", "bar")
    blob.save(writeToRepository)
    assert(repository.contains(blob.hash))
    repository(blob.hash) shouldEqual "bar"
  }

  it should "load from working directory" in {
    var workingDirectory = mutable.Map.empty[String, String]
    val blob = Blob("/foo", "bar")
    workingDirectory(blob.name.drop(1)) = blob.content
    val getFromWorkingDirectory = (name: String) => Some(workingDirectory(name))
    val loadedBlob = Blob.loadFromWD("/foo", getFromWorkingDirectory)
    loadedBlob shouldEqual Some(blob)
  }

  it should "load from repository" in {
    var repository = mutable.Map.empty[String, String]
    val blob = Blob("/foo", "bar")
    repository(blob.hash) = blob.content
    val getFromRepository = (hash: String) => Some(repository(hash))
    val loadedBlob = Blob.loadFromRepo(blob.name, blob.hash, getFromRepository)
    loadedBlob shouldEqual Some(blob)
  }
}
