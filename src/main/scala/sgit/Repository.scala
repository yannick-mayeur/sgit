package sgit
import java.io.File
import scala.annotation.tailrec

case class Repository(sgitFile: File)

object Repository {
  def initRepository(path: String): Unit = {
    if (isInRepository(path)) {} else {
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

  val createSgitPath = (path: String) => s"$path${File.separator}.sgit"

  def isInRepository(path: String): Boolean = {
    @tailrec
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

  def getRepository(path: String) = {
    @tailrec
    def loop(currentPath: String): Option[Repository] = {
      val currentSgitFile = new File(createSgitPath(currentPath))
      val currentFile = new File(currentPath)
      if (currentFile.getParent() != null) {
        if (currentSgitFile.exists()) {
          Some(Repository(currentSgitFile))
        } else {
          loop(currentFile.getParent())
        }
      } else {
        if (currentSgitFile.exists()) {
          Some(Repository(currentSgitFile))
        } else {
          None
        }
      }
    }
    loop(path)
  }

  val getRepositoryRootFolder = (repository: Repository) => {
    val canonicalPathSgitFile = repository.sgitFile.getCanonicalPath()
    canonicalPathSgitFile
      .split(File.separator)
      .dropRight(1)
      .mkString(File.separator)
  }
}
