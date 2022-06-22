package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import cats.data._
import cats.effect.kernel.Sync
import cats.effect.{Async, Concurrent, Resource}
import cats.implicits._
import cats.{~>, Monad, MonadThrow, Parallel}
import co.topl.algebras.{Store, StoreReader}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.{
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra,
  MempoolAlgebra,
  TransactionSemanticValidationAlgebra,
  TransactionSyntaxValidationAlgebra
}
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BlockchainPeerHandlerImpl {

  /**
   * A naive `BlockchainPeerHandler` which fetches each block that is notified by a remote peer.  If the fetched
   * block's parent is missing on this node, it is also requested from the peer. Once all missing blocks have been
   * fetched, the original block is signaled out to the program using the provided `onBlockReceived` callback
   */
  object FetchAllBlocks {

    def make[F[_]: Async: Concurrent: Parallel: Logger: FToFuture](
      headerStore:           Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:             Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:      Store[F, TypedIdentifier, Transaction],
      blockIdTree:           ParentChildTree[F, TypedIdentifier],
      onBlockReceived:       Sink[BlockV2, _],
      onTransactionReceived: Sink[Transaction, _],
      blockHeights:          EventSourcedState[F, Long => F[Option[TypedIdentifier]]],
      localChain:            LocalChainAlgebra[F],
      slotDataStore:         StoreReader[F, TypedIdentifier, SlotData]
    )(implicit system:       ActorSystem[_]): F[BlockchainPeerHandler[F]] =
      Sync[F].delay(
        (
          client =>
            Sync[F].defer(
              for {
                getLocalBlockIdAtHeight <- (
                  (height: Long) =>
                    localChain.head
                      .map(_.slotId.blockId)
                      .flatMap(blockHeights.stateAt)
                      .ap(height.pure[F])
                      .flatten
                      .flatMap(
                        OptionT
                          .fromOption[F](_)
                          .toRight(
                            new IllegalStateException(
                              show"Unable to derive block height state for height=$height"
                            )
                          )
                          .rethrowT
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
                blocksReceivedBackgroundF = Async[F].fromFuture(
                  client.remotePeerAdoptions
                    .flatMap(
                      _.mapAsyncF(1)(blockProcessor)
                        .alsoTo(Flow[Option[BlockV2]].collect { case Some(t) => t }.to(onBlockReceived))
                        .toMat(Sink.ignore)(Keep.right)
                        .liftTo[F]
                    )
                )
                transactionsReceivedBackgroundF = Async[F].fromFuture(
                  client.remoteTransactionNotifications
                    .flatMap(
                      _.tapAsyncF(1)(id =>
                        client.remotePeer.flatMap(peer =>
                          Logger[F].info(show"Remote peer=${peer.remoteAddress} observed transaction id=$id")
                        )
                      )
                        .mapAsyncF(1)(id =>
                          transactionStore
                            .contains(id)
                            .ifM(
                              none[Transaction].pure[F],
                              OptionT(client.getRemoteTransaction(id)).getOrNoSuchElement(id).map(_.some)
                            )
                        )
                        .collect { case Some(transaction) =>
                          transaction
                        }
                        .tapAsyncF(1)(transaction =>
                          client.remotePeer.flatMap(peer =>
                            Logger[F].info(
                              show"Retrieved transaction id=${transaction.id.asTypedBytes} from remote peer=${peer.remoteAddress}"
                            )
                          )
                        )
                        .alsoTo(onTransactionReceived)
                        .toMat(Sink.ignore)(Keep.right)
                        .liftTo[F]
                    )
                )
                commonAncestorBackgroundF = Async[F].fromFuture(
                  Source
                    .tick(10.seconds, 10.seconds, ())
                    .mapAsyncF(1)(_ =>
                      traceAndLogCommonAncestor(client, getLocalBlockIdAtHeight, currentHeightLookup, slotDataStore)
                    )
                    .toMat(Sink.ignore)(Keep.right)
                    .liftTo[F]
                )
                _ <- NonEmptyChain(
                  blocksReceivedBackgroundF,
                  transactionsReceivedBackgroundF,
                  commonAncestorBackgroundF
                ).parSequence
              } yield ()
            )
        ): BlockchainPeerHandler[F]
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
        maybeBlock <- (OptionT.fromOption[F](maybeCurrentHeader), OptionT(bodyStore.get(id)))
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
      (body: BlockBodyV2) => body.toList.traverse(fetchTransaction[F](client, transactionStore))

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
  }

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  object V2 {

    def make[F[_]: Async: Parallel: FToFuture: RunnableGraphToF: Logger](
      localChain:                    LocalChainAlgebra[F],
      headerValidation:              BlockHeaderValidationAlgebra[F],
      transactionSyntaxValidation:   TransactionSyntaxValidationAlgebra[F],
      transactionSemanticValidation: TransactionSemanticValidationAlgebra[F],
      bodySyntaxValidation:          BodySyntaxValidationAlgebra[F],
      bodySemanticValidation:        BodySemanticValidationAlgebra[F],
      slotDataStore:                 Store[F, TypedIdentifier, SlotData],
      headerStore:                   Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:                     Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:              Store[F, TypedIdentifier, Transaction],
      mempool:                       MempoolAlgebra[F],
      broadcastBlock:                TypedIdentifier => F[Unit],
      broadcastTransaction:          TypedIdentifier => F[Unit]
    )(implicit mat:                  Materializer) =
      Async[F].delay {
        new BlockchainPeerHandler[F] {
          def usePeer(client: BlockchainPeerClient[F]): F[Unit] =
            processBlockNotifications(
              localChain,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              slotDataStore,
              headerStore,
              bodyStore,
              transactionStore
            )(client)
              .parProduct(
                processTransactionNotifications(
                  transactionSyntaxValidation,
                  transactionSemanticValidation,
                  transactionStore,
                  mempool,
                  broadcastTransaction
                )(client)
              )
              .void
        }
      }

    def processBlockNotifications[F[_]: Async: FToFuture: Logger: RunnableGraph ~> *[_]](
      localChain:             LocalChainAlgebra[F],
      headerValidation:       BlockHeaderValidationAlgebra[F],
      bodySyntaxValidation:   BodySyntaxValidationAlgebra[F],
      bodySemanticValidation: BodySemanticValidationAlgebra[F],
      slotDataStore:          Store[F, TypedIdentifier, SlotData],
      headerStore:            Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:              Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:       Store[F, TypedIdentifier, Transaction]
    )(client:                 BlockchainPeerClient[F])(implicit mat: Materializer): F[Unit] =
      for {
        _adoptions <- client.remotePeerAdoptions
        adoptions  <- _adoptions.withCancel[F]
        _fetchAndValidateMissingHeaders = fetchAndValidateMissingHeaders(
          client,
          headerValidation,
          slotDataStore,
          headerStore
        ) _
        _fetchAndValidateMissingBodies = fetchAndValidateMissingBodies(
          client,
          bodySyntaxValidation,
          bodySemanticValidation,
          slotDataStore,
          headerStore,
          bodyStore,
          transactionStore
        ) _
        _ <- adoptions
          .mapAsyncF(1)(id =>
            slotDataStore
              .contains(id)
              .ifM(
                Logger[F].info(show"Ignoring already-known block header id=$id"),
                for {
                  slotData <- OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id)
                  tine <- buildSlotDataTine(
                    slotDataStore,
                    id => OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id.show)
                  )(slotData)
                  _ <- tine.traverse(slotData => slotDataStore.put(slotData.slotId.blockId, slotData))
                  _ <-
                    localChain
                      .isWorseThan(tine.last)
                      .ifM(
                        // The case where the remote tine is better than the local tine,
                        // but we first need to fetch and validate all of the data
                        for {
                          _ <- _fetchAndValidateMissingHeaders(id)
                          _ <- _fetchAndValidateMissingBodies(id)
                          _ <-
                            // After fetching and validating all of the data, re-run the chain preference process
                            localChain
                              .isWorseThan(tine.last)
                              .ifM(
                                // And finally, adopt the remote peer's tine
                                localChain.adopt(Validated.Valid(tine.last)) >>
                                Logger[F].info(
                                  show"Adopted head block id=${tine.last.slotId.blockId} height=${tine.last.height} slot=${tine.last.slotId.slot}"
                                ),
                                Logger[F].info(show"Ignoring weaker (or equal) block header id=$id")
                              )
                        } yield (),
                        // The case where the remote tine can be ignored
                        Logger[F].info(show"Ignoring weaker (or equal) block header id=$id")
                      )
                } yield ()
              )
          )
          .toMat(Sink.ignore)(Keep.right)
          .liftFutureTo[F]
      } yield ()

    private def fetchAndValidateMissingHeaders[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      client:           BlockchainPeerClient[F],
      headerValidation: BlockHeaderValidationAlgebra[F],
      slotDataStore:    Store[F, TypedIdentifier, SlotData],
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2]
    )(from:             TypedIdentifier) =
      missingValues(
        headerStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingHeaders =>
          Source
            .fromIterator(() => missingHeaders.iterator)
            .mapAsyncF(1)(blockId =>
              OptionT(client.getRemoteHeader(blockId))
                .filter(_.id.asTypedBytes === blockId)
                .getOrNoSuchElement(blockId.show)
                .tupleLeft(blockId)
            )
            .tapAsyncF(1) { case (blockId, header) =>
              EitherT(headerStore.get(header.parentHeaderId).flatMap(headerValidation.validate(_, header)))
                .leftSemiflatTap(error => Logger[F].warn(show"Received invalid block header id=$blockId error=$error"))
                .leftMap(errors => new IllegalArgumentException(error.show))
                .rethrowT >>
              headerStore.put(blockId, header)
            }
            .toMat(Sink.ignore)(Keep.right)
            .liftFutureTo[F]
            .void
        )

    private def fetchAndValidateMissingBodies[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      client:           BlockchainPeerClient[F],
      validateBody:     BlockV2 => F[ValidatedNec[String, BlockV2]],
      slotDataStore:    Store[F, TypedIdentifier, SlotData],
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:        Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore: Store[F, TypedIdentifier, Transaction]
    )(from:             TypedIdentifier) =
      missingValues(
        bodyStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingBodyIds =>
          Source
            .fromIterator(() => missingBodyIds.iterator)
            .mapAsyncF(1)(blockId =>
              OptionT(client.getRemoteBody(blockId)) // TODO: Verify against Header#txRoot
                .getOrNoSuchElement(blockId.show)
                .tupleLeft(blockId)
            )
            .mapAsyncF(1) { case (blockId, body) =>
              body.toList
                .traverse(transactionId =>
                  OptionT(transactionStore.get(transactionId))
                    .getOrElseF(
                      OptionT(client.getRemoteTransaction(transactionId))
                        .filter(_.id.asTypedBytes === transactionId)
                        .getOrNoSuchElement(transactionId.show)
                        .flatTap(transaction => transactionStore.put(transactionId, transaction))
                    )
                )
                .map((blockId, body, _))
            }
            .tapAsyncF(1) { case (blockId, body, _) => bodyStore.put(blockId, body) }
            .tapAsyncF(1) { case (blockId, body, _) =>
              headerStore
                .getOrRaise(blockId)
                .map(header => BlockV2(header, body))
                .flatMap(block =>
                  EitherT(validateBody(block).map(_.toEither))
                    .leftSemiflatTap(errors =>
                      Logger[F].warn(show"Received invalid block body id=$blockId errors=$errors")
                    )
                    .leftMap(errors => new IllegalArgumentException(errors.show))
                    .rethrowT
                    // TODO: Delete the bad transaction, body, and header
                    .void
                )
            }
            .toMat(Sink.ignore)(Keep.right)
            .liftFutureTo[F]
        )

    private def missingValues[F[_]: Monad](
      existsLocally: TypedIdentifier => F[Boolean],
      parentOf:      TypedIdentifier => F[TypedIdentifier]
    )(from:          TypedIdentifier) =
      (NonEmptyChain(from), false)
        .iterateUntilM { case (ids, _) =>
          parentOf(ids.head).flatMap(parentId => existsLocally(parentId).map((ids.prepend(parentId), _)))
        }(_._2)
        .map(_._1)

    private def buildTine[F[_]: Monad, Value](
      store:           Store[F, TypedIdentifier, Value],
      fetchRemoteData: TypedIdentifier => F[Value],
      parentOf:        (TypedIdentifier, Value) => F[TypedIdentifier]
    )(from:            (TypedIdentifier, Value)) =
      (NonEmptyChain(from), false)
        .iterateUntilM { case (data, _) =>
          parentOf(data.head._1, data.head._2)
            .flatMap(parentId =>
              store
                .contains(parentId)
                .ifM(
                  (data, true).pure[F],
                  fetchRemoteData(parentId).map(parentData => (data.prepend(parentData), false))
                )
            )
        }(_._2)
        .map(_._1.map(_._2))

    private def buildSlotDataTine[F[_]: Monad](
      store:               Store[F, TypedIdentifier, SlotData],
      fetchRemoteSlotData: TypedIdentifier => F[SlotData]
    )(from:                SlotData) =
      buildTine[F, SlotData](store, fetchRemoteSlotData, ((_, data) => data.parentSlotId.blockId.pure[F]))(
        (from.slotId.blockId, from)
      )

    private def processTransactionNotifications[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      transactionSyntaxValidation:   TransactionSyntaxValidationAlgebra[F],
      transactionSemanticValidation: TransactionSemanticValidationAlgebra[F],
      transactionStore:              Store[F, TypedIdentifier, Transaction],
      mempool:                       MempoolAlgebra[F],
      broadcastTransaction:          TypedIdentifier => F[Unit]
    )(client:                        BlockchainPeerClient[F])(implicit materializer: Materializer) =
      client.remoteTransactionNotifications
        .flatMap(_.withCancel[F])
        .flatMap(source =>
          Async[F].fromFuture(
            source
              .mapAsyncF(1)(id => OptionT(client.getRemoteTransaction(id)).getOrNoSuchElement(id.show))
              .mapAsyncF(1)(transaction =>
                EitherT(transactionSyntaxValidation.validate(transaction).map(_.toEither))
                  .leftSemiflatMap(errors =>
                    Logger[F].warn(
                      show"Received syntactically invalid transaction id=${transaction.id.asTypedBytes} errors=$errors"
                    )
                  )
                  .value
              )
              .collect { case Right(transaction) => transaction }
              .tapAsyncF(1)(transaction =>
                Logger[F]
                  .info(show"Inserting syntactically valid transaction id=${transaction.id.asTypedBytes} into Store")
              )
              .tapAsyncF(1)(transaction => transactionStore.put(transaction.id, transaction))
              .tapAsyncF(1)(transaction =>
                Logger[F]
                  .info(
                    show"Inserting syntactically valid transaction id=${transaction.id.asTypedBytes} into Mempool"
                  )
              )
              .tapAsyncF(1)(transaction => mempool.add(transaction.id))
              .tapAsyncF(1)(transaction =>
                Logger[F].info(show"Broadcasting transaction id=${transaction.id.asTypedBytes} to peers")
              )
              .tapAsyncF(1)(transaction => broadcastTransaction(transaction.id))
              .toMat(Sink.ignore)(Keep.right)
              .liftTo[F]
          )
        )
        .void
  }
}
