package sgit

case class Branch(
    name: String,
    head: String
) {
  def save(writeBranchToRepository: Option[String] => (String => Unit)) = {
    writeBranchToRepository(Some(name))(head)
  }
}

object Branch {}
