package sgit
import scala.annotation.tailrec
import sgit.fileIO.FileHelpers
import java.{util => ju}

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
      .getHead(this)
      .map(Head.fromXml(_))
      .flatMap(_.getCommit(this))
  }

  def updateHead(hash: String) = {
    FileHelpers
      .getHead(this)
      .map(Head.fromXml(_))
      .map(_.update(hash, this))
      .orElse(Some(Head.initialCommit(hash, this)))
  }

  def getLog() = {
    getHead()
      .map { commit =>
        commit.getLog()
      }
      .getOrElse("")
  }

  def cleanWorkingDirectory(): Option[Repository] = {
    Diff.isDiffWithWorkingDirecory(this) && Diff.isDiffWithLastCommit(this) match {
      case true =>
        getStage()
          .getStagedFiles()
          .foreach(_.foreach { path =>
            FileHelpers.deleteFile(path.drop(1))
          })
        Some(this)
      case false =>
        None
    }
  }

  def fillWith(ref: String): Unit = {
    val commit = FileHelpers
      .getCommit(this, ref)
      .map(("commit", _))
      .orElse {
        FileHelpers
          .getContent(
            FileHelpers.branchPath(this, ref)
          )
          .flatMap(FileHelpers.getCommit(this, _))
          .map(("branch", _))
      }
    commit
      .flatMap(commit => Commit.fromXml(this, commit._2))
      .foreach { commit =>
        commit.loadAllFiles(this)
        Stage(Some(commit.rootTree)).save(this)
      }
    commit.foreach {
      case (category, _) =>
        Head(category, ref).save(this)
    }
  }

  def addFiles(pathToFiles: Seq[String]): Unit = {
    this
      .getStage()
      .addFiles(
        this,
        pathToFiles
          .map(file => FileHelpers.getCanonical(file))
          .flatMap(file => FileHelpers.listDirectoryFiles(file))
      )
  }

  def checkout(ref: String): Unit = {
    cleanWorkingDirectory()
      .map(_.fillWith(ref))
  }

  def commit(message: String): Unit = {
    getStage().treeOpt
      .map(
        tree =>
          Commit(
            tree,
            ju.Calendar.getInstance().getTime().toString(),
            message,
            getHead()
          )
      )
      .foreach(_.save(this))
  }

  def createBranch(name: String): Unit = {
    getHead()
      .map { commit =>
        FileHelpers.writeFile(
          FileHelpers.branchPath(this, name),
          commit.hash
        )
      }
  }
}

object Repository {
  def initRepository(path: String): Option[Repository] = {
    if (getRepository(path).isEmpty) {
      val hasMadeSgit = FileHelpers.createFolder(".sgit")
      val files =
        List("HEAD", "STAGE", s"branches${FileHelpers.separator}master")
      val folders = List("tags", "trees", "blobs", "branches", "commits")
      val hasCreatedFolders = folders
        .map(
          file =>
            FileHelpers.createFolder(s".sgit${FileHelpers.separator}$file")
        )
        .reduce(_ && _)
      val hasCreatedFiles = files
        .map(
          (file) =>
            FileHelpers.createFile(s".sgit${FileHelpers.separator}$file")
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
