package sgit
import scala.annotation.tailrec
import sgit.fileIO.FileHelpers

case class Repository private (sgitFilePath: String) {
  val getRepositoryRootFolder = () => {
    sgitFilePath
      .split(FileHelpers.separator)
      .dropRight(1)
      .mkString(FileHelpers.separator)
  }

  def getPathInRepositoryFor(filePath: String): String = {
    filePath.replaceFirst(s"$sgitFilePath", "")
  }

  def isInRepository(path: String): Boolean = {
    path.contains(sgitFilePath)
  }

  def getStage() = {
    Stage.loadStage(this)
  }

  def getHead(): Option[Commit] = {
    FileHelpers
      .getContent(
        s"$sgitFilePath${FileHelpers.separator}.sgit${FileHelpers.separator}HEAD"
      )
      .flatMap(FileHelpers.getCommit(this, _))
      .flatMap(Commit.fromXml(this, _))
  }

  def getLog() = {
    getHead()
      .map { commit =>
        commit.getLog()
      }
      .getOrElse("")
  }

  def cleanWorkingDirectory(): Option[Repository] = {
    Diff.isDiffWithWorkingDirecory(this) match {
      case false =>
        getStage()
          .getStagedFiles()
          .map(_.map { path =>
            println(path)
            FileHelpers.deleteFile(path.drop(1))
          })
        Some(this)
      case true =>
        None
    }
  }

  def fillWith(ref: String): Option[Repository] = {
    FileHelpers
      .getCommit(this, ref)
      .flatMap(xml => Commit.fromXml(this, xml))
      .map(commit => commit.loadAllFiles())
    None
  }
}

object Repository {
  def initRepository(path: String): Option[Repository] = {
    if (getRepository(path).isEmpty) {
      val hasMadeSgit = FileHelpers.createFolder(".sgit")
      val files = List("HEAD", "STAGE")
      val folders = List("tags", "trees", "blobs", "branches", "commits")
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
      if (hasCreatedFiles && hasCreatedFolders) Some(Repository(path)) else None
    } else {
      None
    }
  }

  def getRepository(path: String) = {
    def loop(folders: List[String], currentPath: String): Option[Repository] = {
      folders match {
        case x :: xs =>
          val newPath = s"$currentPath${FileHelpers.separator}$x"
          val sgitPath = s"${newPath}${FileHelpers.separator}.sgit"
          if (FileHelpers.exists(sgitPath)) {
            Some(Repository(newPath))
          } else {
            loop(xs, newPath)
          }
        case _ => None
      }
    }
    val folders = path.split(FileHelpers.separator).toList.drop(1)
    loop(folders, "")
  }
}
