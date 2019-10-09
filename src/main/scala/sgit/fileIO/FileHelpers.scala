package sgit.fileIO

import java.io.File
import scala.annotation.tailrec
import sgit.Repository
import java.io.BufferedReader
import java.io.FileReader
import scala.io.Source

object FileHelpers {
  val separator = File.separator
  val repoOpt = Repository.getRepository(System.getProperty("user.dir"))

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

  def getTree(hash: String): Option[scala.xml.Node] = {
    repoOpt.map { repo =>
      val file = new File(s"${repo.sgitFilePath}/trees/$hash")
      scala.xml.XML.loadFile(file)
    }
  }

  def getBlob(hash: String): Option[String] = {
    repoOpt.map { repo =>
      val path = s"${repo.sgitFilePath}/blobs/$hash"
      var res = ""
      for (line <- Source.fromFile(path).getLines) {
        res += line
      }
      res
    }
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
