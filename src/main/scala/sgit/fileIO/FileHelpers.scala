package sgit.fileIO

import java.io.File
import scala.annotation.tailrec
import sgit.Repository
import java.io.BufferedReader
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable
import scala.util.Try

object FileHelpers {
  val separator = File.separator

  val blobPath = (repository: Repository, hash: String) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}blobs${FileHelpers.separator}${hash}"

  val treePath = (repository: Repository, hash: String) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}trees${FileHelpers.separator}${hash}"

  val commitPath = (repository: Repository, hash: String) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}commits${FileHelpers.separator}${hash}"

  val branchPath = (repository: Repository, name: String) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}branches${FileHelpers.separator}${name}"

  val stagePath = (repository: Repository) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}STAGE"

  val headPath = (repository: Repository) =>
    s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}HEAD"

  def getCanonical(currentDirPath: String, filePath: String) = {
    new File(s"$currentDirPath$separator$filePath").getCanonicalPath()
  }

  def createFolder(path: String) = {
    new File(path).mkdir()
  }

  def createFile(path: String) = {
    new File(path).createNewFile()
  }

  def deleteFile(path: String) = new File(path).delete()

  def exists(path: String) = {
    new File(path).exists()
  }

  def sgitFileExists(paths: List[String]) = {
    paths
      .map { path =>
        new File(s"$path${separator}.sgit").exists()
      }
      .reduce(_ || _)
  }

  def formatXml(xml: scala.xml.Node) = {
    (new scala.xml.PrettyPrinter(80, 4)).format(xml).toString
  }

  def getTree(repository: Repository, hash: String): Option[scala.xml.Node] = {
    val file = new File(
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}trees${FileHelpers.separator}$hash"
    )
    if (file.exists() && file.isFile()) Some(scala.xml.XML.loadFile(file))
    else None
  }

  def getHead(repository: Repository): Option[scala.xml.Node] = {
    Try {
      val file = new File(
        s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}HEAD"
      )
      scala.xml.XML.loadFile(file)
    }.toOption
  }

  def getCommit(
      repository: Repository,
      hash: String
  ): Option[scala.xml.Node] = {
    Try {
      val file = new File(
        s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}commits${FileHelpers.separator}$hash"
      )
      scala.xml.XML.loadFile(file)
    }.toOption
  }

  def getContent(path: String) = {
    if (new File(path).exists()) {
      var res = mutable.ListBuffer[String]()
      for (line <- Source.fromFile(path).getLines) {
        res.append(line)
      }
      Some(res.mkString("\n"))
    } else {
      None
    }
  }

  def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  def getBlob(repository: Repository, hash: String): Option[String] = {
    val path =
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}blobs${FileHelpers.separator}$hash"
    var res = mutable.ListBuffer[String]()
    Try {
      for (line <- Source.fromFile(path).getLines) {
        res += line
      }
      res.mkString("\n")
    }.toOption
  }

  def listDirectoryFiles(path: String): Array[String] = {
    def loop(root: File): Array[String] = {
      root.listFiles().flatMap { file =>
        if (file.getName() != ".sgit" && file.getName() != ".git") {
          if (file.isDirectory()) loop(file)
          else Array(file.getPath())
        } else {
          Array[String]()
        }
      }
    }
    val root = new File(path)
    if (root.isDirectory()) loop(root) else Array(path)
  }
}
