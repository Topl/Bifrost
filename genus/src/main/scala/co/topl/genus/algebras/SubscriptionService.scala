package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.algebras.SubscriptionService.{CreateRequest, CreateSubscriptionFailure}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.types.BlockHeight

/**
 * Subscribes to data from a data source and provides options for filtering results.
 * @tparam F the effect-ful type of the final value
 * @tparam T the type of data returned
 * @tparam Filter the type of values that can be provided to filter the results
 */
trait SubscriptionService[F[_], T] {

  /**
   * Creates a new subscription for data values of type [[T]].
   * @param request request parameters for creating a subscription
   * @return either a subscription as a value of [[Source]] or a [[CreateSubscriptionFailure]]
   */
  def create[Filter: MongoFilter](
    request: CreateRequest[Filter]
  ): EitherT[F, CreateSubscriptionFailure, Source[T, NotUsed]]
}

object SubscriptionService {

  /**
   * A request to create a subscription with some additional options.
   * @param filter a filter to apply to the subscribed results
   * @param startFromHeight only subscribes to data in blocks on the chain at or beyond the given height
   * @param confirmationDepth the minimum distance between the block containing subscribed data and the current head
   *                          of the chain
   * @tparam Filter the type of values that can be provided for filtering
   */
  case class CreateRequest[Filter](filter: Filter, startFromHeight: Option[BlockHeight], confirmationDepth: Int)

  /**
   * A failure that occurred while creating or maintaining the subscription.
   */
  sealed trait CreateSubscriptionFailure

  object CreateSubscriptionFailures {

    /**
     * The query contained invalid options.
     * @param reasons the reasons why the query was invalid
     */
    case class InvalidRequest(reasons: NonEmptyChain[String]) extends CreateSubscriptionFailure

    /**
     * A failure occurred with creating a subscription to the data store.
     * @param message the reason why the connection failed
     */
    case class DataConnectionFailure(message: String) extends CreateSubscriptionFailure
  }
}
