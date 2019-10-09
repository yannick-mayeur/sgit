package sgit
import scopt.OParser
import scala.xml._

case class Config(
    command: String = "",
    files: Seq[String] = Seq(),
    branch: String = "",
    commit: String = "",
    tag: String = ""
)

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
      cmd("add")
        .text("Add files to the stage area")
        .action((_, c) => c.copy(command = "add"))
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
            .action((f, c) => c.copy(files = c.files :+ f))
        ),
      cmd("test")
        .text("test")
        .action((_, c) => c.copy(command = "test")),
      checkConfig { c =>
        c match {
          case Config("", _, _, _, _) => failure("No command given")
          case _                      => success
        }
      }
    )
  }
  val currentDirPath = System.getProperty("user.dir")
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config match {
        case Config("init", _, _, _, _) =>
          Repository.initRepository(currentDirPath)
        case Config("status", _, _, _, _) =>
          FileStatus.printStatus(currentDirPath)
        case Config("add", files, _, _, _) =>
          files.foreach(f => println(f))
        case Config("test", _, _, _, _) =>
        case _                          =>
      }
    case _ =>
    // arguments are bad, error message is displayed
  }
}
