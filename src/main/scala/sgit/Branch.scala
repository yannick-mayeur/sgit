package sgit

case class Branch(
    name: String,
    head: Commit
) {
  def save(writeBranchToRepository: Option[String] => (String => Unit)) = {
    writeBranchToRepository(Some(name))(head.hash)
  }
}

object Branch {}
