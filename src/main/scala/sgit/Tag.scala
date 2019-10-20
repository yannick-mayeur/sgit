package sgit

case class Tag(
    name: String,
    commit: Commit
) {
  def save(writeTagToRepository: Option[String] => (String => Unit)) = {
    writeTagToRepository(Some(name))(commit.hash)
  }
}
