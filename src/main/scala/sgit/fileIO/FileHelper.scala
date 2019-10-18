package sgit.fileIO
import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import java.io.BufferedWriter
import java.io.FileWriter

class FileHelper {

  val separator = File.separator

  def getContent(base: String)(fileName: String) = {
    Try {
      var res = mutable.ListBuffer[String]()
      for (line <- Source
             .fromFile(s"$base$separator$separator$fileName")
             .getLines) {
        res.append(line)
      }
      res.mkString("\n")
    }.toOption
  }

  def getXMLContent(base: String)(fileName: String) = {
    val file = new File(s"$base$separator$fileName")
    Try(scala.xml.XML.loadFile(file)).toOption
  }

  def writeFile(
      path: String
  )(fileName: Option[String])(content: String): Unit = {
    val file = new File(s"$path${fileName.map(separator + _).getOrElse("")}")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  def writeXMLToFile(
      path: String
  )(fileName: Option[String])(content: xml.Node): Unit = {
    writeFile(path)(fileName)(
      (new scala.xml.PrettyPrinter(80, 4)).format(content).toString
    )
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

  def getCanonical(filePath: String) = {
    new File(s"$filePath").getCanonicalPath()
  }

  def listDirectoryFiles(path: String): List[String] = {
    def loop(root: File): List[String] = {
      root.listFiles().toList.flatMap { file =>
        if (file.getName() != ".sgit" && file.getName() != ".git") {
          if (file.isDirectory()) loop(file)
          else List(file.getPath())
        } else {
          List[String]()
        }
      }
    }
    val root = new File(path)
    if (root.isDirectory()) loop(root) else List(path)
  }
}
