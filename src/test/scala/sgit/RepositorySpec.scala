package sgit

import org.scalatest._
import org.mockito.IdiomaticMockito
import scala.collection.mutable
import sgit.fileIO.FileHelper
import java.io.File
import org.mockito.ArgumentMatchersSugar
import scala.util.Try

class RepositrySpec
    extends FlatSpec
    with BeforeAndAfter
    with Matchers
    with IdiomaticMockito
    with ArgumentMatchersSugar {
  val separator = File.separator
  val files: mutable.Map[String, String] = mutable.Map.empty
  val currentDirPath: String = "/repo"
  implicit val aFileHelper = mock[FileHelper]

  before {
    files("/repo/foo") = "bar"
    ((base: String, fileName: String) => {
      Try(files(s"$base$separator$fileName")).toOption
    }) willBe answered by aFileHelper.getContent(*)(*)

    ((base: String, fileName: String) => {
      Try(xml.XML.loadString(files(s"$base$separator$fileName"))).toOption
    }) willBe answered by aFileHelper.getXMLContent(*)(*)

    ((path: String, fileName: Option[String], content: String) => {
      files(s"$path${fileName.map(separator + _).getOrElse("")}") = content
    }) willBe answered by aFileHelper.writeFile(*)(*)(*)

    ((path: String, fileName: Option[String], content: xml.Node) => {
      aFileHelper.writeFile(path)(fileName)(content.toString)
    }) willBe answered by aFileHelper.writeXMLToFile(*)(*)(*)

    ((path: String) => {
      files(path) = ""
      true
    }) willBe answered by aFileHelper.createFile(*)

    ((path: String) => {
      files.remove(s"$currentDirPath$separator$path")
      true
    }) willBe answered by aFileHelper.deleteFile(*)

    aFileHelper.createFolder(*) returns true
    aFileHelper.separator returns separator
    ((path: String) => {
      files.keys.toList
        .filter(_.startsWith(path))
        .filterNot(_.contains(".sgit"))
        .map(_.replaceFirst(path, ""))
    }) willBe answered by aFileHelper.listDirectoryFiles(*)
  }

  after {
    files.clear()
    reset(aFileHelper)
  }

  "Repository" should "create the .sgit structure" in {
    val repo = Repository.initRepository(currentDirPath)
    aFileHelper.createFolder(s"$currentDirPath$separator.sgit") was called
    aFileHelper.createFolder(*) wasCalled sixTimes
    aFileHelper.createFile(*) wasCalled threeTimes
  }

  it should "check if .sgit exists" in {
    Repository.initRepository(currentDirPath)
    aFileHelper.exists(*) wasCalled atLeastOnce
  }

  it should "give path in repo for file" in {
    val repo = Repository("/foo/bar")
    val file = "/foo/bar/baz"
    val path = repo.getPathInRepositoryFor(file)
    path shouldBe "/baz"
  }

  it should "add file to stage" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.getStage().treeOpt shouldBe None
    repo.addFiles(Seq("/repo/foo"))
    repo.getStage().treeOpt shouldBe defined
  }

  it should "create a branch" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("message")
    repo.createBranch("dev")
    Try(files("/repo/.sgit/branches/dev")).toOption shouldBe defined
  }

  it should "not create a branch when repo has no commit" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.createBranch("dev")
    Try(files("/repo/.sgit/branches/dev")).toOption shouldBe None
  }

  it should "commit current stage" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("message")
    val res = repo
      .getHeadCommit()
      .map(commit => Some(commit.rootTree) == repo.getStage().treeOpt)
      .getOrElse(false)
    assert(res)
  }

  it should "commit to master" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("message")
    Try(files("/repo/.sgit/branches/master")).toOption shouldBe defined
  }

  it should "checkout another branch" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("c1")
    repo.createBranch("dev")
    repo.checkout("dev")
    files("/repo/foo") = "baz"
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("c2")
    Try(files("/repo/foo")).toOption shouldEqual Some("baz")
    repo.checkout("master")
    Try(files("/repo/foo")).toOption shouldEqual Some("bar")
  }

  it should "create tag" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("c1")
    repo.createTag("v1")
    Try(files("/repo/.sgit/tags/v1")).toOption shouldBe defined
  }

  it should "not create tag if exists" in {
    val repo = Repository.initRepository(currentDirPath).get
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("c1")
    repo.createTag("v1")
    files("/repo/foo") = "baz"
    repo.addFiles(Seq("/repo/foo"))
    repo.commit("c2")
    repo.createTag("v1").toOption shouldBe None
  }
}
