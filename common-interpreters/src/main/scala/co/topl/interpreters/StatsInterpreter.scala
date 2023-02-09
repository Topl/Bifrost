package co.topl.interpreters

import cats.Applicative
import cats.effect.kernel.Sync
import co.topl.algebras.Stats
import io.circe.Json

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

object StatsInterpreter {

  object Eval {
    private val openOptions = List(StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.APPEND)

    def make[F[_]: Sync](path: Path): Stats[F] = { (name, data) =>
      val filePath = Paths.get(path.toString, name + ".csv")
      val contents =
        data.asObject.fold(data.toString())(d => d.toList.map(_._2.toString).mkString(",")) + "\n"
      Sync[F].blocking {
        if (!Files.exists(filePath)) {
          val header = data.asObject.fold("")(d => d.toList.map(_._1).mkString(",")) + "\n"
          Files.write(filePath, (header + contents).getBytes(StandardCharsets.UTF_8), openOptions: _*)
        } else {
          Files.write(filePath, contents.getBytes(StandardCharsets.UTF_8), openOptions: _*)
        }
      }
    }
  }

  object Noop {

    def make[F[_]: Applicative]: Stats[F] =
      (_: String, _: Json) => Applicative[F].unit
  }
}
