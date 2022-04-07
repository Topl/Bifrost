package co.topl.genus.programs

import cats.effect.Async
import cats.implicits._
import co.topl.genus.algebras.HttpServerAlg

object RunServerProgram {

  object Eval {

    def make[F[_]: Async](server: HttpServerAlg[F]): F[Unit] =
      for {
        binding <- server.run
        _       <- println(s"Genus server running at ${binding.localAddress}").pure[F]
      } yield ()
  }
}
