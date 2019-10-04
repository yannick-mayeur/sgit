package sgit
import scopt.OParser

case class Config(
  command: String = "",
  filename: String = "",
  branch: String = "",
  commit: String = "",
  tag: String = "")

object SgitParser extends App {
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("scopt", "4.x"),
      help("help").text("prints this usage text"),
      cmd("init")
        .text("Initialize a repository")
        .action((_, c) => c.copy(command = "init")),
      checkConfig { c => c match {
        case Config("", _, _, _, _) => failure("No command given")
        case _ => success
      }}
    )
  }
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config match {
        case Config(init, _, _, _, _) => println("init")
        case _ =>
      }
      println(config)
    case _ =>
      // arguments are bad, error message is displayed
  }
}
