package sgit
import scala.annotation.tailrec
import sgit.fileIO.FileHelpers

case class Repository(sgitFilePath: String)

object Repository {
  def initRepository(path: String): Unit = {
    if (isInRepository(path)) {} else {
      val hasMadeSgit = FileHelpers.createFolder(".sgit")
      val files = List("HEAD", "STAGE")
      val folders = List("tags", "trees", "blobs", "branches")
      val hasCreatedFolders = files
        .map(
          (file) =>
            FileHelpers.createFile(s".sgit${FileHelpers.separator}$file")
        )
        .reduce(_ && _)
      val hasCreatedFiles = folders
        .map(
          file =>
            FileHelpers.createFolder(s".sgit${FileHelpers.separator}$file")
        )
        .reduce(_ && _)
    }
  }

  val createSgitPath = (path: String) => s"$path${FileHelpers.separator}.sgit"

  def isInRepository(path: String): Boolean = {
    getRepository(path) match {
      case Some(_) => true
      case _       => false
    }
  }

  def getRepository(path: String) = {
    def loop(folders: List[String], currentPath: String): Option[Repository] = {
      folders match {
        case x :: xs =>
          val newPath = currentPath + x
          val sgitPath = s"${newPath}.sgit"
          if (FileHelpers.exists(sgitPath)) {
            Some(Repository(sgitPath))
          } else {
            loop(xs, newPath)
          }
        case _ => None
      }
    }

    val folders = path.split(FileHelpers.separator).toList.map(_ + "/")
    loop(folders, "")
  }

  val getRepositoryRootFolder = (repository: Repository) => {
    val canonicalPathSgitFile = repository.sgitFilePath
    canonicalPathSgitFile
      .split(FileHelpers.separator)
      .dropRight(1)
      .mkString(FileHelpers.separator)
  }
}
