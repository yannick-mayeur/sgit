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

  def printStatus(repository: Repository): Unit = {
    val stagedOpt = repository.getStage().getStagedFiles()

    val getBlobContent = (path: String) =>
      Blob.load(path, repository).map(_.content)
    val modified = Diff.getDiffBetweenStageAnd(getBlobContent, repository)

    val getContentFor = (path: String) =>
      repository.getHead().flatMap(_.rootTree.getBlobContentAt(path))
    val toBecommitted = Diff.getDiffBetweenStageAnd(getContentFor, repository)

    val untracked = stagedOpt
      .map { staged =>
        FileHelpers
          .listDirectoryFiles(repository.sgitFilePath)
          .map(path => repository.getPathInRepositoryFor(path))
          .toList
          .filterNot(path => staged.contains(path))
      }
      .orElse {
        Some(
          FileHelpers
            .listDirectoryFiles(repository.sgitFilePath)
            .map(path => repository.getPathInRepositoryFor(path))
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
