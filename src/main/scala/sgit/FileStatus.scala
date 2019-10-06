package sgit
import java.io.File

object FileStatus {
  def getAllFiles(path: String): Array[String] = {
    val root = new File(path)
    root.listFiles().flatMap { file =>
      if (file.getName() != ".git") {
        if (file.isDirectory()) getAllFiles(file.getPath)
        else Array(file.getPath().drop(2))
      } else {
        Array[String]()
      }
    }
  }

}
