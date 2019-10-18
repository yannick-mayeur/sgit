package sgit
import java.security.MessageDigest
import java.io.File

object Helper {
  def getHashFor(string: String) = {
    MessageDigest
      .getInstance("SHA-1")
      .digest(string.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
  }

  val separator = File.separator
}
