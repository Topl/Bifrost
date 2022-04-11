package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats._
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.networking.p2p.ConnectedPeer
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

/**
 * A client which can be used by a local node to retrieve blockchain data from a remote peer
 */
trait BlockchainPeerClient[F[_]] {
  def remotePeer: F[ConnectedPeer]
  def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getRemoteHeader(id:              TypedIdentifier): F[Option[BlockHeaderV2]]
  def getRemoteBody(id:                TypedIdentifier): F[Option[BlockBodyV2]]
  def getRemoteTransaction(id:         TypedIdentifier): F[Option[Transaction]]
  def getRemoteBlockIdAtHeight(height: Long, localBlockId: Option[TypedIdentifier]): F[Option[TypedIdentifier]]

  def findCommonAncestor(
    getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
    currentHeight:           () => F[Long]
  )(implicit syncF:          Sync[F], loggerF: Logger[F]): F[TypedIdentifier] =
    Sync[F]
      .defer(
        for {
          initialHeight <- currentHeight()
          intersectionOpt <- narySearch(getLocalBlockIdAtHeight, getRemoteBlockIdAtHeight, Ratio(2, 3))
            .apply(1L, initialHeight)
          intersection <- OptionT
            .fromOption[F](intersectionOpt)
            .getOrElseF(
              MonadThrow[F].raiseError(new IllegalStateException("Unable to find common ancestor with remote peer"))
            )
        } yield intersection
      )

  /**
   * A "divide-and-conquer" search, similar to binary search, but the "array" is divided into potentially unequal parts.
   * Binary search cuts the search space in half, whereas this search cuts the search space into sizes determined
   * by the given `searchSpaceTarget`.
   *
   * In cases where you expect to find the value closer to the "end" of the "array", a larger `searchSpaceTarget` will
   * converge on a result faster.
   * TODO: Could this be a "function" that "accelerates"?  Meaning, if the search space recurses to the right several
   * times, could the search detect that and increase the searchSpaceTarget ratio to be more aggressive?
   */
  private def narySearch[T: Eq](
    getLocal:          Long => F[T],
    getRemote:         (Long, Option[T]) => F[Option[T]],
    searchSpaceTarget: Ratio
  )(implicit monadF:   Monad[F], loggerF: Logger[F]): (Long, Long) => F[Option[T]] = {
    lazy val f: (Long, Long, Option[T]) => F[Option[T]] = (min, max, ifNone) =>
      Logger[F].debug(show"Recursing common ancestor search in bounds=($min, $max)") >>
      (min === max)
        .pure[F]
        .ifM(
          ifTrue = getLocal(min)
            .flatMap(localValue =>
              OptionT(getRemote(min, localValue.some))
                .filter(_ === localValue)
                .orElse(OptionT.fromOption[F](ifNone))
                .value
            ),
          ifFalse = for {
            targetHeight <- (min + ((max - min) * searchSpaceTarget.toDouble).floor.round).pure[F]
            localValue   <- getLocal(targetHeight)
            remoteValue  <- getRemote(targetHeight, localValue.some)
            result <- remoteValue
              .filter(_ === localValue)
              .fold(f(min, targetHeight, ifNone))(remoteValue => f(targetHeight + 1, max, remoteValue.some))
          } yield result
        )
    (min, max) => f(min, max, None)
  }

}
