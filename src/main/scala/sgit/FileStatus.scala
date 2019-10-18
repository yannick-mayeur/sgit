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

  def printDiff(repository: Repository) = {
    val stagedOpt = repository.getStage().getStagedFiles()
    val modified = stagedOpt.map(_.flatMap { path =>
      val stagedContentOpt = repository.getStage().getContentFor(path)
      val commitContentOpt = FileHelpers.getContent(path.drop(1))
      val diffs = for {
        stagedContent <- stagedContentOpt
        commitContent <- commitContentOpt
      } yield {
        val elem1 = stagedContent.split("\n")
        val elem2 = commitContent.split("\n")
        val d = Diff.getDiffBetweenElements(elem1, elem2)
        d
      }
      println(path)
      diffs
        .filter(
          _.changes
            .map(change => change._1 == "> " || change._1 == "< ")
            .reduce(_ || _)
        )
    }).foreach(println)
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
