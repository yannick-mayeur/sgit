package sgit

import org.scalatest._
import scala.xml._

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
}
