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
    val stagedOpt = repository.getStage().getStagedFiles().map(_.map(_.drop(1)))
    val paths = FileHelpers
      .listDirectoryFiles(repository.sgitFilePath)
      .map(path => repository.getPathInRepositoryFor(path).drop(1))
    val untracked = paths.filterNot { path =>
      stagedOpt
        .map(staged => staged.contains(path))
        .reduceOption(_ || _)
        .getOrElse(false)
    }
    val trackedPartition = stagedOpt.map(_.partition { path =>
      val workingTreeContent =
        FileHelpers.getContent(path).getOrElse("").split("\n").toSeq
      val diffs = repository
        .getStage()
        .getContentFor(path)
        .map(
          content =>
            Diff.getDiffBetweenElements(
              content.split("\n").toSeq,
              workingTreeContent
            )
        )
      diffs.foreach(println)
      diffs.exists(
        diff =>
          diff.changes
            .map(change => change._1 == "< " || change._1 == "> ")
            .reduce(_ || _)
      )
    })
    println("Changes to be committed:")
    stagedOpt.foreach(_.foreach(println))
    println("Changes not staged for commit:")
    println("  (use \"sgit add <file>...\" to update what will be committed)")
    trackedPartition.map(_._1).foreach(_.foreach(println))
    println(
      "untracked files:\n  (use \"sgit add <file>...\" to include in what will be committed)"
    )
    untracked.foreach(println)
  }

}
