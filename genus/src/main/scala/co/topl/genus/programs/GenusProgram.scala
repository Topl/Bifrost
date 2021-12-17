package co.topl.genus.programs

import cats.effect.Async
import cats.implicits._
import co.topl.genus.algebras.HttpServer

object GenusProgram {

  object Mock {

    def make[F[_]: Async](server: HttpServer[F]): F[Unit] =
      for {
        binding <- server.run
        _       <- println(s"Genus server running at ${binding.localAddress}").pure[F]
      } yield ()
  }
}
