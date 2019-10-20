package sgit

trait Reference {
  def verboseToString(): String
  def getName(): String
  def save(writeReferenceToRepository: Option[String] => (String => Unit)): Unit
}
