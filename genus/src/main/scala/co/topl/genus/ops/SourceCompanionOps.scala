package co.topl.genus.ops

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.~>
import co.topl.genus.ops.implicits._

import scala.concurrent.Future
import scala.language.implicitConversions

final class SourceCompanionOps(private val companion: Source.type) extends AnyVal {

  def asyncF[F[_]: *[_] ~> Future, T](from: F[T]): Source[T, NotUsed] = Source.future(from.mapFunctor[Future])
}

object SourceCompanionOps {

  trait ToOps {

    implicit def sourceCompanionOpsFromValue(companion: Source.type): SourceCompanionOps = new SourceCompanionOps(
      companion
    )
  }

  object ops extends ToOps
}
