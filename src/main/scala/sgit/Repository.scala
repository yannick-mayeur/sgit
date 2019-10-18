package sgit
import scala.annotation.tailrec
import sgit.fileIO.FileHelper
import java.{util => ju}

case class Repository private (sgitFilePath: String)(
    implicit fileHelper: FileHelper
) {
  lazy val getStageContent = () =>
    fileHelper.getContent(s"$sgitFilePath${fileHelper.separator}.sgit")("STAGE")
  def writeStageToRepository =
    fileHelper.writeFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}STAGE"
    )(None) _
  lazy val getBlobContent = fileHelper.getContent(
    s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}blobs"
  ) _
  def writeBlobsToRepository =
    fileHelper.writeFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}blobs"
    ) _
  lazy val getBranchContent = fileHelper.getContent(
    s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}branches"
  ) _
  def writeBranchToRepository =
    fileHelper.writeFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}branches"
    ) _
  lazy val getTreeXmlFrom = fileHelper.getXMLContent(
    s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}trees"
  ) _
  def writeTreeToRepository =
    fileHelper.writeXMLToFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}trees"
    ) _
  lazy val getCommitXmlFrom = fileHelper.getXMLContent(
    s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}commits"
  ) _
  def writeCommitToRepository =
    fileHelper.writeXMLToFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}commits"
    ) _
  lazy val getContentFromWD = fileHelper.getContent(sgitFilePath) _
  def writeBranchToWD = fileHelper.writeFile(sgitFilePath) _

  lazy val getHeadContent = () =>
    fileHelper.getXMLContent(
      s"$sgitFilePath${fileHelper.separator}.sgit"
    )("HEAD")
  def writeHeadToRepository =
    fileHelper.writeXMLToFile(
      s"$sgitFilePath${fileHelper.separator}.sgit${fileHelper.separator}HEAD"
    )(None) _

  val getRepositoryRootFolder = () => {
    sgitFilePath
      .split(fileHelper.separator)
      .dropRight(1)
      .mkString(fileHelper.separator)
  }

  def getPathInRepositoryFor(filePath: String): String = {
    filePath.replaceFirst(s"$sgitFilePath", "")
  }

  def isInRepository(path: String): Boolean = {
    path.contains(sgitFilePath)
  }

  def getStage() = {
    Stage.loadStage(getStageContent, getTreeXmlFrom, getBlobContent)
  }

  def getHead(): Option[Commit] = {
    getHeadContent()
      .map(Head.fromXml(_))
      .flatMap(
        _.getCommit(
          getCommitXmlFrom,
          getTreeXmlFrom,
          getBlobContent,
          getBranchContent
        )
      )
  }

  def updateHead(hash: String) = {
    getHeadContent()
      .map(Head.fromXml(_))
      .map(_.update(writeHeadToRepository, writeBranchToRepository, hash))
      .orElse(
        Some(
          Head
            .initialCommit(writeHeadToRepository, writeBranchToRepository, hash)
        )
      )
  }

  def getLog() = {
    getHead()
      .map { commit =>
        commit.getLog()
      }
      .getOrElse("")
  }

  def cleanWorkingDirectory(): Option[Repository] = {
    Diff.isDiffWithWorkingDirecory(getContentFromWD, getStage()) && getHead()
      .map(Diff.isDiffWithLastCommit(_, getStage()))
      .getOrElse(true) match {
      case true =>
        getStage()
          .getStagedFiles()
          .foreach(_.foreach { path =>
            fileHelper.deleteFile(path.drop(1))
          })
        Some(this)
      case false =>
        None
    }
  }

  def fillWith(ref: String): Unit = {
    val commit = getCommitXmlFrom(ref)
      .map(("commit", _))
      .orElse {
        getBranchContent(ref)
          .flatMap(getCommitXmlFrom(_))
          .map(("branch", _))
      }
    commit
      .flatMap(
        commit =>
          Commit.fromXml(
            getCommitXmlFrom,
            getTreeXmlFrom,
            getBlobContent,
            commit._2
          )
      )
      .foreach { commit =>
        commit.loadAllFiles(writeBranchToWD)
        Stage(Some(commit.rootTree)).save(writeStageToRepository)
      }
    commit.foreach {
      case (category, _) =>
        Head(category, ref).save(writeHeadToRepository)
    }
  }

  def addFiles(pathToFiles: Seq[String]): Unit = {
    this
      .getStage()
      .addFiles(
        writeStageToRepository,
        writeTreeToRepository,
        writeBlobsToRepository,
        getContentFromWD,
        pathToFiles
          .map(file => fileHelper.getCanonical(file))
          .flatMap(file => fileHelper.listDirectoryFiles(file))
          .map(file => getPathInRepositoryFor(file))
      )
  }

  def checkout(ref: String): Unit = {
    cleanWorkingDirectory()
      .map(_.fillWith(ref))
  }

  def commit(message: String): Unit = {
    val commit = getStage().treeOpt
      .map(
        tree =>
          Commit(
            tree,
            ju.Calendar.getInstance().getTime().toString(),
            message,
            getHead()
          )
      )
    commit.foreach(
      _.save(
        writeCommitToRepository,
        writeTreeToRepository,
        writeBlobsToRepository
      )
    )
    commit.map(commit => updateHead(commit.hash))
  }

  def createBranch(name: String): Unit = {
    getHead()
      .map { commit =>
        writeBranchToRepository(Some(name))(commit.hash)
      }
  }

  def printStatus(): Unit = {
    val stagedOpt = getStage().getStagedFiles()

    val getBlobContent = (path: String) =>
      Blob.loadFromWD(path, getContentFromWD).map(_.content)
    val modified = Diff.getDiffBetweenStageAnd(getBlobContent, getStage())

    val getContentFor = (path: String) =>
      getHead().flatMap(_.rootTree.getBlobContentAt(path))
    val toBecommitted = Diff.getDiffBetweenStageAnd(getContentFor, getStage())

    val untracked = stagedOpt
      .map { staged =>
        fileHelper
          .listDirectoryFiles(sgitFilePath)
          .map(path => getPathInRepositoryFor(path))
          .toList
          .filterNot(path => staged.contains(path))
      }
      .orElse {
        Some(
          fileHelper
            .listDirectoryFiles(sgitFilePath)
            .map(path => getPathInRepositoryFor(path))
            .toList
        )
      }

    println("Changes to be committed:")
    toBecommitted.map(_.foreach(println))
    println("Changes not staged for commit:")
    println("  (use \"sgit add <file>...\" to update what will be committed)")
    modified.map(_.foreach(println))
    println(
      "untracked files:\n  (use \"sgit add <file>...\" to include in what will be committed)"
    )
    untracked.map(_.foreach(println))
  }
}

object Repository {
  implicit val fileHelper = new FileHelper()

  def initRepository(path: String): Option[Repository] = {
    if (getRepository(path).isEmpty) {
      val hasMadeSgit = fileHelper.createFolder(".sgit")
      val files =
        List("HEAD", "STAGE", s"branches${fileHelper.separator}master")
      val folders = List("tags", "trees", "blobs", "branches", "commits")
      val hasCreatedFolders = folders
        .map(
          file => fileHelper.createFolder(s".sgit${fileHelper.separator}$file")
        )
        .reduce(_ && _)
      val hasCreatedFiles = files
        .map(
          (file) => fileHelper.createFile(s".sgit${fileHelper.separator}$file")
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
          val newPath = s"$currentPath${fileHelper.separator}$x"
          val sgitPath = s"${newPath}${fileHelper.separator}.sgit"
          if (fileHelper.exists(sgitPath)) {
            Some(Repository(newPath))
          } else {
            loop(xs, newPath)
          }
        case _ => None
      }
    }
    val folders = path.split(fileHelper.separator).toList.drop(1)
    loop(folders, "")
  }
}
