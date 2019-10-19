package sgit

import org.scalatest._
import scala.xml._
import scala.collection.mutable
import scala.util.Try

class HeadSpec extends FlatSpec with Matchers {
  "The Head object" should "save to xml" in {
    val xml = Head("foo", "bar").toXml()
    xml shouldEqual <Head category="foo">bar</Head>
  }

  it should "load from xml" in {
    val xml = <Head category="foo">bar</Head>
    val head = Head("foo", "bar")
    Head.fromXml(xml) shouldEqual head
  }

  it should "get head commit when detatched" in {
    val repositoryCommits = mutable.Map.empty[String, xml.Node]
    val commit =
      Commit(Tree("", Seq(), Seq(Blob("/foo", "bar"))), "time", "message", None)
    repositoryCommits(commit.hash) = commit.toXml()
    val getCommitXmlFrom =
      (hash: String) => Try(repositoryCommits(hash)).toOption
    val getTreeFromXmlFrom =
      (str: String) => Some(Tree("", Seq(), Seq(Blob("/foo", "bar"))).toXml())
    val getBlobContentFrom = (str: String) => Some("bar")
    val getBranchContentFrom = (str: String) => None
    val head = Head("commit", commit.hash)

    val res = head.getCommit(
      getCommitXmlFrom,
      getTreeFromXmlFrom,
      getBlobContentFrom,
      getBranchContentFrom
    )
    res shouldEqual Some(commit)
  }

  it should "get head commit when on branch" in {
    val repositoryCommits = mutable.Map.empty[String, xml.Node]
    val repositoryBranches = mutable.Map.empty[String, String]
    val commit =
      Commit(Tree("", Seq(), Seq(Blob("/foo", "bar"))), "time", "message", None)
    val branch = Branch("master", commit.hash)
    repositoryCommits(commit.hash) = commit.toXml()
    repositoryBranches(branch.name) = branch.head
    val getCommitXmlFrom =
      (hash: String) => Try(repositoryCommits(hash)).toOption
    val getTreeFromXmlFrom =
      (str: String) => Some(Tree("", Seq(), Seq(Blob("/foo", "bar"))).toXml())
    val getBlobContentFrom = (str: String) => Some("bar")
    val getBranchContentFrom =
      (name: String) => Try(repositoryBranches(name)).toOption
    val head = Head("branch", branch.name)

    val res = head.getCommit(
      getCommitXmlFrom,
      getTreeFromXmlFrom,
      getBlobContentFrom,
      getBranchContentFrom
    )
    res shouldEqual Some(commit)
  }

  it should "update when detached" in {
    val head = Head("commit", "hash1")
    var repositoryHead = head.toXml().toString()
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository =
      (name: Option[String]) => (content: String) => ()

    val newHead =
      head.update(writeHeadToRepository, writeBranchToRepository, "hash2")
    newHead shouldEqual Head("commit", "hash2")
    repositoryHead shouldEqual Head("commit", "hash2").toXml.toString()
  }

  it should "update when on branch" in {
    val head = Head("branch", "master")
    var repositoryHead = head.toXml().toString()
    val repositoryBranches = mutable.Map.empty[String, String]
    repositoryBranches("master") = "hash1"
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository = (name: Option[String]) =>
      (content: String) => name.foreach(repositoryBranches(_) = content)

    val newHead =
      head.update(writeHeadToRepository, writeBranchToRepository, "hash2")
    newHead shouldEqual Head("branch", "master")
    repositoryHead shouldEqual Head("branch", "master").toXml.toString()
    repositoryBranches("master") shouldEqual "hash2"
  }

  "The Head Companion" should "initialze on master branch" in {
    var repositoryHead = ""
    val repositoryBranches = mutable.Map.empty[String, String]
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository = (name: Option[String]) =>
      (content: String) => name.foreach(repositoryBranches(_) = content)

    val res =
      Head.initialCommit(writeHeadToRepository, writeBranchToRepository, "hash")
    res shouldEqual Head("branch", "master")
    repositoryBranches("master") shouldEqual "hash"
  }

}
