package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.StoreReader
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import co.topl.models._
import co.topl.networking.p2p.ConnectedPeer

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
  )(implicit syncF: Sync[F]): F[TypedIdentifier] =
    Sync[F].defer(
      for {
        head               <- localChain.head
        remoteAtHeadHeight <- getRemoteBlockIdAtHeight(head.height, head.slotId.blockId.some)
        // TODO: This is a naive approach which performs a reverse-linear search
        (_, intersectionOpt) <-
          //
          (head, remoteAtHeadHeight).iterateUntilM { case (local, remote) =>
            val nextHeight = local.height - 1
            OptionT(
              blockHeights
                .stateAt(head.slotId.blockId)
                .ap(nextHeight.pure[F])
                .flatten
            )
              .flatMapF(slotDataStore.get)
              .getOrElseF(
                MonadThrow[F].raiseError(new IllegalStateException(s"Unable to find local block at height=$nextHeight"))
              )
              .flatMap(localSlotData =>
                getRemoteBlockIdAtHeight(nextHeight, localSlotData.slotId.blockId.some).tupleLeft(localSlotData)
              )
          } { case (local, remote) =>
            //
            remote.contains(local.slotId.blockId)
          }
        intersection <- OptionT
          .fromOption[F](intersectionOpt)
          .getOrElseF(
            MonadThrow[F].raiseError(new IllegalStateException("Unable to find common ancestor with remote peer"))
          )
      } yield intersection
    )
}
