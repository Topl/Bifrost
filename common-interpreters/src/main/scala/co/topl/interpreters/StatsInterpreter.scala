package co.topl.interpreters

import cats.effect.kernel.Sync
import co.topl.algebras.Stats

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

object StatsInterpreter {

  object Eval {
    private val openOptions = List(StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.APPEND)

    def make[F[_]: Sync](path: Path): Stats[F] = { (name, data) =>
      val filePath = Paths.get(path.toString, name + ".csv")
      val contents =
        data.asObject.fold(data.toString())(d => d.toList.map(_._2.toString).mkString(",")) + "\n"
      Sync[F].blocking(Files.write(filePath, contents.getBytes(StandardCharsets.UTF_8), openOptions: _*))
    }
  }
}
