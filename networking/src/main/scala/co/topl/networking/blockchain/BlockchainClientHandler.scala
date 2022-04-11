package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.effect.{Async, Concurrent}
import cats.implicits._
import cats.{MonadThrow, Parallel}
import co.topl.algebras.{Store, StoreReader}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait BlockchainClientHandler[F[_]] {
  def useClient(client: BlockchainPeerClient[F]): F[Unit]
}

object BlockchainClientHandler {

  /**
   * A naive `BlockchainClientHandler` which fetches each block that is notified by a remote peer.  If the fetched
   * block's parent is missing on this node, it is also requested from the peer. Once all missing blocks have been
   * fetched, the original block is signaled out to the program using the provided `onBlockReceived` callback
   */
  object FetchAllBlocks {

    def make[F[_]: Async: Concurrent: Parallel: Logger: FToFuture](
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:        Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore: Store[F, TypedIdentifier, Transaction],
      blockIdTree:      ParentChildTree[F, TypedIdentifier],
      onBlockReceived:  Sink[BlockV2, _],
      blockHeights:     EventSourcedState[F, TypedIdentifier, Long => F[Option[TypedIdentifier]]],
      localChain:       LocalChainAlgebra[F],
      slotDataStore:    StoreReader[F, TypedIdentifier, SlotData]
    )(implicit system:  ActorSystem[_]): F[BlockchainClientHandler[F]] =
      Sync[F].delay(
        (
          client =>
            Sync[F].defer(
              for {
                getLocalBlockIdAtHeight <- (
                  (height: Long) =>
                    localChain.head
                      .map(_.slotId.blockId)
                      .flatMap(blockHeights.stateAt(_))
                      .ap(height.pure[F])
                      .flatten
                      .flatMap(
                        OptionT
                          .fromOption[F](_)
                          .getOrElseF(
                            MonadThrow[F]
                              .raiseError(
                                new IllegalStateException(
                                  show"Unable to derive block height state for height=$height"
                                )
                              )
                          )
                      )
                ).pure[F]
                currentHeightLookup <- (() => localChain.head.map(_.height)).pure[F]
                _ <- traceAndLogCommonAncestor(client, getLocalBlockIdAtHeight, currentHeightLookup, slotDataStore)
                blockProcessor = processRemoteBlockNotification[F](
                  client,
                  headerStore,
                  bodyStore,
                  transactionStore,
                  blockIdTree
                ) _
                implicit0(ec: ExecutionContext) = system.executionContext
                f1 = Async[F].fromFuture(
                  client.remotePeerAdoptions
                    .flatMap(
                      _.mapAsyncF(1)(blockProcessor)
                        .alsoTo(Flow[Option[BlockV2]].collect { case Some(t) => t }.to(onBlockReceived))
                        .toMat(Sink.ignore)(Keep.right)
                        .liftTo[F]
                    )
                )
                f2 = Async[F].fromFuture(
                  Source
                    .tick(10.seconds, 10.seconds, ())
                    .mapAsyncF(1)(_ =>
                      traceAndLogCommonAncestor(client, getLocalBlockIdAtHeight, currentHeightLookup, slotDataStore)
                    )
                    .toMat(Sink.ignore)(Keep.right)
                    .liftTo[F]
                )
                _ <- f1.parProduct(f2)
              } yield ()
            )
        ): BlockchainClientHandler[F]
      )

    private def traceAndLogCommonAncestor[F[_]: Sync: Logger](
      client:                  BlockchainPeerClient[F],
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    ) =
      client.remotePeer
        .flatMap(peer =>
          Logger[F].info(show"Starting common ancestor trace with remote=${peer.remoteAddress}") >>
          client
            .findCommonAncestor(getLocalBlockIdAtHeight, currentHeight)
            .flatTap(ancestor =>
              OptionT(slotDataStore.get(ancestor))
                .getOrNoSuchElement(ancestor)
                .flatMap(slotData =>
                  Logger[F].info(
                    show"Traced remote=${peer.remoteAddress} common ancestor to" +
                    show" id=$ancestor" +
                    show" height=${slotData.height}" +
                    show" slot=${slotData.slotId.slot}"
                  )
                )
            )
        )
        .void
        .handleErrorWith(
          Logger[F].error(_)("Common ancestor trace failed")
        )

    private def processRemoteBlockNotification[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:        Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore: Store[F, TypedIdentifier, Transaction],
      blockIdTree:      ParentChildTree[F, TypedIdentifier]
    )(id:               TypedIdentifier) =
      for {
        _ <-
          client.remotePeer.flatMap(peer =>
            Logger[F].info(show"Remote=${peer.remoteAddress} peer adopted block id=$id")
          )
        maybeCurrentHeader <- headerStore.get(id)
        _ <- (id, maybeCurrentHeader)
          // Recursively fetch the remote header+body+transactions until a common ancestor is found
          .iterateWhileM[F] { case (id, _) =>
            fetchHeader(client, headerStore, blockIdTree)(id)
              .productL(fetchBody(client, bodyStore)(id).flatMap(fetchTransactions(client, transactionStore)).void)
              .flatMap(header => headerStore.get(header.parentHeaderId).tupleLeft(header.parentHeaderId))
          }(_._2.isEmpty)
        maybeBlock <- (OptionT.fromOption[F](maybeCurrentHeader), (OptionT(bodyStore.get(id))))
          .mapN(BlockV2.apply)
          .orElse(
            (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id)))
              .mapN((header, body) => BlockV2(header, body))
              .semiflatTap(_ =>
                client.remotePeer
                  .flatMap(peer => Logger[F].info(show"Processing remote=${peer.remoteAddress} block id=$id"))
              )
          )
          .value
      } yield maybeBlock

    private def fetchHeader[F[_]: Async: Concurrent: Logger: FToFuture](
      client:      BlockchainPeerClient[F],
      headerStore: Store[F, TypedIdentifier, BlockHeaderV2],
      blockIdTree: ParentChildTree[F, TypedIdentifier]
    )(id:          TypedIdentifier) =
      OptionT(headerStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](
              Sync[F].defer(
                client.remotePeer
                  .flatMap(peer => Logger[F].info(show"Requesting remote=${peer.remoteAddress} header id=$id"))
              )
            )
            .flatMap(_ =>
              OptionT(client.getRemoteHeader(id))
                .flatMapF(header =>
                  if (header.id.asTypedBytes === id) header.some.pure[F]
                  else
                    MonadThrow[F].raiseError[Option[BlockHeaderV2]](
                      new IllegalArgumentException(show"Remote sent block id=${header.id} but we requested id=${id}")
                    )
                )
                .semiflatTap(_ =>
                  client.remotePeer
                    .flatMap(peer => Logger[F].info(show"Inserting remote=${peer.remoteAddress} header id=$id"))
                )
                .flatTapNone(
                  client.remotePeer
                    .flatMap(peer => Logger[F].info(show"Remote=${peer.remoteAddress} did not possess header id=$id"))
                )
                .semiflatTap(header => headerStore.put(header.id, header))
                .semiflatTap(header => blockIdTree.associate(header.id, header.parentHeaderId))
            )
        )
        .getOrNoSuchElement(id.show)

    private def fetchBody[F[_]: Async: Concurrent: Logger: FToFuture](
      client:    BlockchainPeerClient[F],
      bodyStore: Store[F, TypedIdentifier, BlockBodyV2]
    )(id:        TypedIdentifier) =
      OptionT(bodyStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](
              Sync[F].defer(
                client.remotePeer
                  .flatMap(peer => Logger[F].info(show"Requesting remote=${peer.remoteAddress} body id=$id"))
              )
            )
            .flatMap(_ =>
              OptionT(client.getRemoteBody(id))
                // TODO: Verify the transaction IDs associated with this body match the txRoot of the header
                .semiflatTap(_ =>
                  client.remotePeer
                    .flatMap(peer => Logger[F].info(show"Inserting remote=${peer.remoteAddress} body id=$id"))
                )
                .flatTapNone(
                  client.remotePeer
                    .flatMap(peer => Logger[F].info(show"Remote=${peer.remoteAddress} did not possess body id=$id"))
                )
                .semiflatTap(bodyStore.put(id, _))
            )
        )
        .getOrNoSuchElement(id.show)

    private def fetchTransactions[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      transactionStore: Store[F, TypedIdentifier, Transaction]
    ) =
      (body: BlockBodyV2) => body.traverse(fetchTransaction[F](client, transactionStore))

    private def fetchTransaction[F[_]: Async: Concurrent: Logger: FToFuture](
      client:           BlockchainPeerClient[F],
      transactionStore: Store[F, TypedIdentifier, Transaction]
    ) = { (id: TypedIdentifier) =>
      OptionT(transactionStore.get(id))
        .orElse(
          OptionT
            .liftF[F, Unit](Sync[F].defer(Logger[F].info(show"Requesting transaction id=$id"))) >>
          OptionT(client.getRemoteTransaction(id))
            // TODO:  Verify the locally computed transaction ID against `id`
            .semiflatTap(_ =>
              client.remotePeer
                .flatMap(peer => Logger[F].info(show"Inserting remote=${peer.remoteAddress} transaction id=$id"))
            )
            .flatTapNone(
              client.remotePeer
                .flatMap(peer => Logger[F].info(show"Remote=${peer.remoteAddress} did not possess transaction id=$id"))
            )
            .semiflatTap(transactionStore.put(id, _))
        )
        .getOrNoSuchElement(id.show)
    }

    implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

      def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
        optionT.getOrElseF(M.raiseError(new NoSuchElementException(id.toString)))
    }
  }
}
