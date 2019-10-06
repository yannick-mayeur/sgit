package sgit
import scopt.OParser
import java.io.File

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
      cmd("status")
        .text("Print status of repository")
        .action((_, c) => c.copy(command = "status")),
      cmd("test")
        .text("test")
        .action((_, c) => c.copy(command = "test")),
      checkConfig { c => c match {
        case Config("", _, _, _, _) => failure("No command given")
        case _ => success
      }}
    )
  }
  val t1 = "abc"
  val t2 = "abd"
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config match {
        case Config("init", _, _, _, _)   => Repository.initRepository(System.getProperty("user.dir"))
        case Config("status", _, _, _, _) => FileStatus.printStatus(new File("."))
        case Config("test", _, _, _, _)   => FileStatus.getAllFiles(new File(System.getProperty("user.dir"))).foreach(println(_))
        case _ =>
      }
    case _ =>
      // arguments are bad, error message is displayed
  }
}
