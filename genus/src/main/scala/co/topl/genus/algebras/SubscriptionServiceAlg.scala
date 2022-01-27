package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateRequest, CreateSubscriptionFailure}

trait SubscriptionServiceAlg[F[_], T, Filter, Token] {
  def create(request: CreateRequest[Filter, Token]): EitherT[F, CreateSubscriptionFailure, Source[T, NotUsed]]
}

object SubscriptionServiceAlg {
  case class CreateRequest[Filter, Token](filter: Option[Filter], resumeToken: Option[Token], confirmationDepth: Long)

  sealed trait CreateSubscriptionFailure

  object CreateSubscriptionFailures {
    case class InvalidRequest(reasons: NonEmptyChain[String]) extends CreateSubscriptionFailure
    case class DataConnectionFailure(message: String) extends CreateSubscriptionFailure
  }
}
