package sgit
import java.io.File
import scala.annotation.tailrec

object Repository {
  def initRepository(path: String): Unit = {
    if (isInRepository(path)) {
      println("tamer")
    } else {
      val hasMadeSgit = new File(".sgit").mkdir()
      val files = List("HEAD", "STAGE")
      val folders = List("tags", "trees", "blobs", "branches")
      val hasCreatedFolders = files
        .map((file) => new File(s".sgit${File.separator}$file").mkdir())
        .reduce(_ && _)
      val hasCreatedFiles = folders
        .map(file => new File(s".sgit${File.separator}$file").createNewFile())
        .reduce(_ && _)
    }
  }

  def isInRepository(path: String): Boolean = {
    val createSgitPath = (path: String) => s"$path${File.separator}.sgit"
    // @tailrec
    def loop(currentPath: String): Boolean = {
      val currentSgitFile = new File(createSgitPath(currentPath))
      val currentFile = new File(currentPath)
      if (currentFile.getParent() != null) {
        currentSgitFile.exists() || loop(currentFile.getParent())
      } else {
        currentSgitFile.exists()
      }
    }
    loop(path)
  }
}
