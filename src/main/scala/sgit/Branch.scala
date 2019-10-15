package sgit
import sgit.fileIO.FileHelpers

case class Branch(
    name: String,
    head: String
) {
  def save(repository: Repository): Unit = {
    FileHelpers.writeFile(
      FileHelpers.branchPath(repository, name),
      head
    )
  }
}

object Branch {}
