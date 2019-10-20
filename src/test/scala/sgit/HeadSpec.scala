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
    val getTagContentFrom = (str: String) => None
    val head = Head("commit", commit.hash)

    val res = head.getCommit(
      getCommitXmlFrom,
      getTreeFromXmlFrom,
      getBlobContentFrom,
      getBranchContentFrom,
      getTagContentFrom
    )
    res shouldEqual Some(commit)
  }

  it should "get head commit when on branch" in {
    val repositoryCommits = mutable.Map.empty[String, xml.Node]
    val repositoryBranches = mutable.Map.empty[String, String]
    val commit =
      Commit(Tree("", Seq(), Seq(Blob("/foo", "bar"))), "time", "message", None)
    val branch = Branch("master", commit)
    repositoryCommits(commit.hash) = commit.toXml()
    repositoryBranches(branch.name) = branch.head.hash
    val getCommitXmlFrom =
      (hash: String) => Try(repositoryCommits(hash)).toOption
    val getTreeFromXmlFrom =
      (str: String) => Some(Tree("", Seq(), Seq(Blob("/foo", "bar"))).toXml())
    val getBlobContentFrom = (str: String) => Some("bar")
    val getBranchContentFrom =
      (name: String) => Try(repositoryBranches(name)).toOption
    val getTagContentFrom = (str: String) => None
    val head = Head("branch", branch.name)

    val res = head.getCommit(
      getCommitXmlFrom,
      getTreeFromXmlFrom,
      getBlobContentFrom,
      getBranchContentFrom,
      getTagContentFrom
    )
    res shouldEqual Some(commit)
  }

  it should "get head commit when on tag" in {
    val repositoryCommits = mutable.Map.empty[String, xml.Node]
    val repositoryTags = mutable.Map.empty[String, String]
    val commit =
      Commit(Tree("", Seq(), Seq(Blob("/foo", "bar"))), "time", "message", None)
    val tag = sgit.Tag("v1", commit)
    repositoryCommits(commit.hash) = commit.toXml()
    repositoryTags(tag.name) = tag.commit.hash
    val getCommitXmlFrom =
      (hash: String) => Try(repositoryCommits(hash)).toOption
    val getTreeFromXmlFrom =
      (str: String) => Some(Tree("", Seq(), Seq(Blob("/foo", "bar"))).toXml())
    val getBlobContentFrom = (str: String) => Some("bar")
    val getTagContentFrom =
      (name: String) => Try(repositoryTags(name)).toOption
    val getBranchContentFrom = (str: String) => None
    val head = Head("tag", tag.name)

    val res = head.getCommit(
      getCommitXmlFrom,
      getTreeFromXmlFrom,
      getBlobContentFrom,
      getBranchContentFrom,
      getTagContentFrom
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

    val commit = Commit(Tree("", Seq(), Seq()), "time", "message", None)
    val newHead =
      head.update(writeHeadToRepository, writeBranchToRepository, commit)
    newHead shouldEqual Head("commit", commit.hash)
    repositoryHead shouldEqual Head("commit", commit.hash).toXml.toString()
  }

  it should "update when detached on tag" in {
    val head = Head("tag", "v1")
    var repositoryHead = head.toXml().toString()
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository =
      (name: Option[String]) => (content: String) => ()

    val commit = Commit(Tree("", Seq(), Seq()), "time", "message", None)
    val newHead =
      head.update(writeHeadToRepository, writeBranchToRepository, commit)
    newHead shouldEqual Head("commit", commit.hash)
    repositoryHead shouldEqual Head("commit", commit.hash).toXml.toString()
  }

  it should "update when on branch" in {
    val head = Head("branch", "master")
    val commit = Commit(Tree("", Seq(), Seq()), "time", "message", None)
    var repositoryHead = head.toXml().toString()
    val repositoryBranches = mutable.Map.empty[String, String]
    repositoryBranches("master") = commit.hash
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository = (name: Option[String]) =>
      (content: String) => name.foreach(repositoryBranches(_) = content)

    val commit2 =
      Commit(Tree("", Seq(), Seq()), "time", "message", Some(commit))
    val newHead =
      head.update(writeHeadToRepository, writeBranchToRepository, commit2)
    newHead shouldEqual Head("branch", "master")
    repositoryHead shouldEqual Head("branch", "master").toXml.toString()
    repositoryBranches("master") shouldEqual commit2.hash
  }

  "The Head Companion" should "initialze on master branch" in {
    var repositoryHead = ""
    val repositoryBranches = mutable.Map.empty[String, String]
    val writeHeadToRepository =
      (xmlTree: xml.Node) => repositoryHead = xmlTree.toString()
    val writeBranchToRepository = (name: Option[String]) =>
      (content: String) => name.foreach(repositoryBranches(_) = content)

    val commit = Commit(Tree("", Seq(), Seq()), "time", "message", None)
    val res =
      Head.initialCommit(writeHeadToRepository, writeBranchToRepository, commit)
    res shouldEqual Head("branch", "master")
    repositoryBranches("master") shouldEqual commit.hash
  }

}
