package sgit

case class Tag(
    name: String,
    hash: String
) {
  def save(writeTagToRepository: Option[String] => (String => Unit)) = {
    writeTagToRepository(Some(name))(hash)
  }
}

object Tag {
  def load(name: String, getBranchContentFrom: String => String) = {
    getBranchContentFrom(name)
  }
}
