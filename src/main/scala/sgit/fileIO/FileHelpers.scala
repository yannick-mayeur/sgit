package sgit.fileIO

import java.io.File
import scala.annotation.tailrec
import sgit.Repository
import java.io.BufferedReader
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter

object FileHelpers {
  val separator = File.separator

  def getCanonical(currentDirPath: String, filePath: String) = {
    new File(s"$currentDirPath$separator$filePath").getCanonicalPath()
  }

  def createFolder(path: String) = {
    new File(path).mkdir()
  }

  def createFile(path: String) = {
    new File(path).createNewFile()
  }

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

  def getTree(repository: Repository, hash: String): scala.xml.Node = {
    val file = new File(
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}trees${FileHelpers.separator}$hash"
    )
    scala.xml.XML.loadFile(file)
  }

  def getContent(path: String) = {
    if (new File(path).exists()) {
      var res = ""
      for (line <- Source.fromFile(path).getLines) {
        res += line
      }
      Some(res)
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

  def getBlob(repository: Repository, hash: String): String = {
    val path =
      s"${repository.sgitFilePath}${FileHelpers.separator}.sgit${FileHelpers.separator}blobs${FileHelpers.separator}$hash"
    var res = ""
    for (line <- Source.fromFile(path).getLines) {
      res += line
    }
    res
  }

  def listDirectoryFiles(path: String): Option[Array[String]] = {
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
    if (root.isDirectory()) Some(loop(root)) else None
  }
}
