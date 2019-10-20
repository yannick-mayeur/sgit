package sgit
import scopt.OParser
import sgit.fileIO.FileHelper

case class Config(
    command: String = "",
    files: Seq[String] = Seq(),
    ref: String = "",
    commit: String = ""
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
      cmd("log")
        .text("Log commit history")
        .action((_, c) => c.copy(command = "log"))
        .children(
          opt[Unit]('p', "full-diff")
            .action((_, c) => c.copy(ref = "full"))
            .text("Log commit history with full diff"),
          opt[Unit]("stat")
            .action((_, c) => c.copy(ref = "stat"))
            .text("Log commit history with diff stats")
        ),
      cmd("diff")
        .text("Print diff beween stage and working directory")
        .action((_, c) => c.copy(command = "diff")),
      cmd("branch")
        .text("Create a new branch")
        .action((_, c) => c.copy(command = "branch"))
        .children(
          opt[Unit]('v', "verbose")
            .action((_, c) => c.copy(ref = "verbose"))
            .text("Log all branches in a verbose manner"),
          arg[String]("<name>")
            .optional()
            .action((b, c) => c.copy(ref = b))
        ),
      cmd("tag")
        .text("Tag current commit")
        .action((_, c) => c.copy(command = "tag"))
        .children(
          arg[String]("<name>")
            .required()
            .action((t, c) => c.copy(ref = t))
        ),
      cmd("checkout")
        .text("Checkout a particular commit, branch or tag")
        .action((_, c) => c.copy(command = "checkout"))
        .children(
          arg[String]("<ref>")
            .required()
            .action((r, c) => c.copy(ref = r))
        ),
      cmd("add")
        .text("Add files to the stage area")
        .action((_, c) => c.copy(command = "add"))
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
            .action((f, c) => c.copy(files = c.files :+ f))
        ),
      cmd("commit")
        .text("Commit staged changes")
        .action((_, c) => c.copy(command = "commit"))
        .children(
          opt[String]('m', "message")
            .required()
            .valueName("<message>")
            .action((x, c) => c.copy(commit = x))
            .text("A message is required to commit")
        ),
      cmd("test")
        .text("test")
        .action((_, c) => c.copy(command = "test")),
      checkConfig { c =>
        c match {
          case Config("", _, _, _) => failure("No command given")
          case _                   => success
        }
      }
    )
  }
  val currentDirPath = System.getProperty("user.dir")
  implicit val fileHelper = new FileHelper()

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config match {
        case Config("init", _, _, _) =>
          Repository.initRepository(currentDirPath)
        case Config("status", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) => repository.printStatus()
            case _                => println("Not in a repository...")
          }
        case Config("add", files, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              repository.addFiles(files)
            case _ => println("Not in a repository...")
          }
        case Config("checkout", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              repository.checkout(config.ref)
            case _ => println("Not in a repository...")
          }
        case Config("commit", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              repository.commit(config.commit)
            case _ => println("Not in a repository...")
          }
        case Config("log", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              println(repository.getLog(config.ref))
            case _ => println("Not in a repository...")
          }
        case Config("diff", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              val diff = repository.getDiff()
              diff.foreach(println)
            case _ => println("Not in a repository...")
          }
        case Config("branch", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              repository.createBranch(config.ref)
            case _ => println("Not in a repository...")
          }
        case Config("tag", _, _, _) =>
          Repository.getRepository(currentDirPath) match {
            case Some(repository) =>
              repository.createTag(config.ref) match {
                case Left(error) => println(error)
                case _           =>
              }
            case _ => println("Not in a repository...")
          }
        case Config("test", _, _, _) => ???
        case _                       =>
      }
    case _ =>
    // arguments are bad, error message is displayed
  }
}
