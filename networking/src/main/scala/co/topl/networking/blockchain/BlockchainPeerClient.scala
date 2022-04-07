package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats._
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.StoreReader
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import co.topl.models.utility.Ratio
import co.topl.models._
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
    blockHeights:   EventSourcedState[F, TypedIdentifier, Long => F[Option[TypedIdentifier]]],
    localChain:     LocalChainAlgebra[F],
    slotDataStore:  StoreReader[F, TypedIdentifier, SlotData]
  )(implicit syncF: Sync[F], loggerF: Logger[F]): F[TypedIdentifier] =
    Sync[F]
      .defer(
        for {
          initialHead <- localChain.head
          getLocalHeight =
            (height: Long) =>
              localChain.head
                .flatMap(head => blockHeights.stateAt(head.slotId.blockId))
                .flatMap(f =>
                  OptionT(f(height))
                    .semiflatMap(slotDataStore.get)
                    .flattenOption
                    .getOrElseF(
                      MonadThrow[F]
                        .raiseError(
                          new IllegalStateException(
                            show"Unable to derive block height state for height=$height"
                          )
                        )
                    )
                )
          intersectionOpt <- narySearch(getLocalHeight, getRemoteBlockIdAtHeight, Ratio(2, 3))
            .apply(1L, initialHead.height)
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
  private def narySearch(
    getLocalSlotData:  Long => F[SlotData],
    getRemoteBlockId:  (Long, Option[TypedIdentifier]) => F[Option[TypedIdentifier]],
    searchSpaceTarget: Ratio
  )(implicit monadF:   Monad[F], loggerF: Logger[F]): (Slot, Slot) => F[Option[TypedIdentifier]] = {
    lazy val f: (Long, Long) => F[Option[TypedIdentifier]] = (min, max) =>
      Logger[F].debug(show"Recursing common ancestor search in bounds=($min, $max)") >>
      (min === max)
        .pure[F]
        .ifM(
          getLocalSlotData(min).flatMap(slotData =>
            OptionT(getRemoteBlockId(min, slotData.slotId.blockId.some)).filter(_ === slotData.slotId.blockId).value
          ),
          for {
            targetHeight  <- (min + ((max - min) * searchSpaceTarget.toDouble).floor.round).pure[F]
            localSlotData <- getLocalSlotData(targetHeight)
            remoteBlockId <- getRemoteBlockId(targetHeight, localSlotData.slotId.blockId.some)
            result <-
              remoteBlockId
                .contains(localSlotData.slotId.blockId)
                .pure[F]
                .ifM(
                  OptionT(f(targetHeight + 1, max))
                    .orElseF(remoteBlockId.pure[F])
                    .value,
                  f(min, targetHeight)
                )
          } yield result
        )
    f
  }

}
