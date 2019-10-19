package sgit

import org.scalatest._

class CommitSpec extends FlatSpec with Matchers {

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
