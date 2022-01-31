package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateRequest, CreateSubscriptionFailure}
import co.topl.genus.types.BlockHeight

trait SubscriptionServiceAlg[F[_], T, Filter] {
  def create(request: CreateRequest[Filter]): EitherT[F, CreateSubscriptionFailure, Source[T, NotUsed]]
}

object SubscriptionServiceAlg {
  case class CreateRequest[Filter](filter: Option[Filter], startFromHeight: Option[BlockHeight], confirmationDepth: Int)

  sealed trait CreateSubscriptionFailure

  object CreateSubscriptionFailures {
    case class InvalidRequest(reasons: NonEmptyChain[String]) extends CreateSubscriptionFailure
    case class DataConnectionFailure(message: String) extends CreateSubscriptionFailure
  }
}
