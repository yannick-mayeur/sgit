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

  def printStatus(repository: Repository) = {
    println(repository)
    println("Changes to be committed:")
    println("Changes not staged for commit:")
    println("  (use \"sgit add <file>...\" to update what will be committed)")
    println(
      "untracked files:\n  (use \"sgit add <file>...\" to include in what will be committed)"
    )
    val paths = FileHelpers.listDirectoryFiles(repository.sgitFilePath)
    val relativePaths =
      paths.map(path => repository.getPathInRepositoryFor(path))
    relativePaths.foreach(println)
  }

}
