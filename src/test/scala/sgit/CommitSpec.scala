package sgit

import org.scalatest._
import sgit.fileIO.FileHelpers

class CommitSpec extends FlatSpec with Matchers {

  "Commit" should "transform to xml" in {
    val tree = Tree("foo", Seq(), Seq())
    val commit = Commit(tree, "time", "message", None)
    val xml =
      <Commit>
        <message>message</message>
        <timestamp>time</timestamp>
        <rootTreeHash>{tree.hash}</rootTreeHash>
        <previousHash></previousHash>
      </Commit>
    FileHelpers.formatXml(commit.toXml()) shouldEqual FileHelpers.formatXml(xml)
  }

  it should "return log" in {
    val tree = Tree("foo", Seq(), Seq())
    val previous = Commit(tree, "time", "message", None)
    val commit = Commit(tree, "time", "message2", Some(previous))
    val log = {
      s"""
commit ${commit.hash}
date: time
message: message2


commit ${previous.hash}
date: time
message: message
"""
    }
    commit.getLog() shouldEqual log
  }
}
