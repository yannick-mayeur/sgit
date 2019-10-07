package sgit
import java.io.File
import java.security.MessageDigest

object FileStatus {
  def getHashFor(string: String) = {
    MessageDigest
      .getInstance("SHA-1")
      .digest(string.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
  }

  def getAllFiles(root: File): Array[String] = {
    root.listFiles().flatMap { file =>
      if (file.getName() != ".sgit" && file.getName() != ".git") {
        if (file.isDirectory()) getAllFiles(file)
        else Array(file.getPath().drop(2))
      } else {
        Array[String]()
      }
    }
  }

  def printStatus(root: File) = {
    println("Changes to be committed:")
    println("Changes not staged for commit:")
    println("  (use \"sgit add <file>...\" to update what will be committed)")
    println(
      "untracked files:\n  (use \"sgit add <file>...\" to include in what will be committed)"
    )
    getAllFiles(root).foreach(println)
  }

  def getPathInRepositoryFor(file: File) {
    Repository.getRepository(file.getPath) match {
      case Some(repository) =>
        val repositoryRootFolder =
          Repository.getRepositoryRootFolder(repository)
        val fileCanonincalPath = file.getCanonicalPath()
        println(
          fileCanonincalPath
            .replaceFirst(repositoryRootFolder + File.separator, "")
        )
      case _ =>
    }
  }
}
