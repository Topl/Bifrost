import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Sync}
import com.typesafe.config.ConfigFactory
import fs2.io.file.{Files, Path}
import fs2.Stream

object Hocon2Json extends IOApp {
  type F[A] = IO[A]

  def run(args: List[String]): IO[ExitCode] =
    for {
      path <- Path(args(0)).pure[F]
      stream = Files[F].walk(path)
      _ <- stream
        .evalMap(path =>
          if (path.toString.endsWith(".json.conf")) {
            Sync[F]
              .delay {
                import com.typesafe.config._
                import com.typesafe.config.impl.Parseable
                val parseable = Parseable.newFile(path.toNioPath.toFile, ConfigParseOptions.defaults())
                val parseValue = classOf[Parseable].getDeclaredMethod("parseValue")
                parseValue.setAccessible(true) // from lightbend/config#460#issuecomment-285662952
                val conf = parseValue.invoke(parseable).asInstanceOf[ConfigValue]
                // Not resolving (yet?) as it's only a config value, not a config (object).
                val json = conf.render(ConfigRenderOptions.concise().setFormatted(true))
                json
              }
              .flatMap(json =>
                Stream
                  .iterable(json.getBytes("UTF-8"))
                  .through(Files[F].writeAll(Path(path.toString.dropRight(5))))
                  .compile
                  .drain
              )
          } else ().pure[F]
        )
        .compile
        .drain
    } yield ExitCode.Success
}
