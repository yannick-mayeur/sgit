package sgit
import java.security.MessageDigest
import sgit.fileIO.FileHelpers

object FileStatus {
  def getHashFor(string: String) = {
    MessageDigest
      .getInstance("SHA-1")
      .digest(string.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
  }

  def printStatus(rootPath: String) = {
    println("Changes to be committed:")
    println("Changes not staged for commit:")
    println("  (use \"sgit add <file>...\" to update what will be committed)")
    println(
      "untracked files:\n  (use \"sgit add <file>...\" to include in what will be committed)"
    )
    val pathsOpt = FileHelpers.listDirectoryFiles(rootPath)
    val relativePathsOpt = pathsOpt.map(
      paths => paths.flatMap(path => getPathInRepositoryFor(path))
    )
    relativePathsOpt.map(_.foreach(println))
  }

  def getPathInRepositoryFor(filePath: String): Option[String] = {
    Repository.getRepository(filePath) match {
      case Some(repository) =>
        val repositoryRootFolder =
          Repository.getRepositoryRootFolder(repository)
        Some(
          filePath
            .replaceFirst(repositoryRootFolder + FileHelpers.separator, "")
        )
      case _ => None
    }
  }
}
