package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import cats.data._
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{Monad, MonadThrow, Parallel, Show}
import co.topl.algebras.{Store, StoreReader}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.BlockHeaderValidationFailure
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.ledger.models.{BodySemanticError, BodySyntaxError, TransactionSemanticError, TransactionSyntaxError}
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

object BlockchainPeerHandlerImpl {

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  object LazyFetch {

    def make[F[_]: Async: Parallel: FToFuture: RunnableGraphToF: Logger](
      localChain:                  LocalChainAlgebra[F],
      headerValidation:            BlockHeaderValidationAlgebra[F],
      transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
      bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
      bodySemanticValidation:      BodySemanticValidationAlgebra[F],
      slotDataStore:               Store[F, TypedIdentifier, SlotData],
      headerStore:                 Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:                   Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      mempool:                     MempoolAlgebra[F],
      getLocalBlockIdAtHeight:     Long => F[TypedIdentifier],
      currentHeight:               () => F[Long],
      blockIdTree:                 ParentChildTree[F, TypedIdentifier]
    )(implicit mat:                Materializer): F[BlockchainPeerHandler[F]] =
      Async[F].delay {
        new BlockchainPeerHandler[F] {
          def usePeer(client: BlockchainPeerClient[F]): F[Unit] =
            traceAndLogCommonAncestor(getLocalBlockIdAtHeight, currentHeight, slotDataStore)(client) >>
            List(
              processBlockNotifications(
                localChain,
                headerValidation,
                bodySyntaxValidation,
                bodySemanticValidation,
                slotDataStore,
                headerStore,
                bodyStore,
                transactionStore,
                blockIdTree
              )(client),
              processTransactionNotifications(
                transactionSyntaxValidation,
                transactionStore,
                mempool
              )(client),
              periodicallyTraceAndLogCommonAncestor(getLocalBlockIdAtHeight, currentHeight, slotDataStore)(client)
            ).parSequence.void
        }
      }

    private def processBlockNotifications[F[_]: Async: FToFuture: Logger: RunnableGraphToF](
      localChain:             LocalChainAlgebra[F],
      headerValidation:       BlockHeaderValidationAlgebra[F],
      bodySyntaxValidation:   BodySyntaxValidationAlgebra[F],
      bodySemanticValidation: BodySemanticValidationAlgebra[F],
      slotDataStore:          Store[F, TypedIdentifier, SlotData],
      headerStore:            Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:              Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:       Store[F, TypedIdentifier, Transaction],
      blockIdTree:            ParentChildTree[F, TypedIdentifier]
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
          .mapAsyncF(1) { id =>
            slotDataStore
              .contains(id)
              .ifM(
                ifTrue = Logger[F].info(show"Ignoring already-known block header id=$id"),
                ifFalse = for {
                  _        <- Logger[F].info(show"Fetching remote SlotData id=$id")
                  slotData <- OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id)
                  // Fetch missing SlotData from the remote peer
                  tine: NonEmptyChain[SlotData] <- buildSlotDataTine(
                    slotDataStore,
                    id => OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id.show)
                  )(slotData)
                  _ <- Logger[F].info(show"Retrieved remote tine length=${tine.length}")
                  // We necessarily need to save Slot Data in the store prior to performing chain "preference"
                  _ <- tine.traverse(slotData =>
                    Logger[F].info(
                      show"Associating child=${slotData.slotId.blockId} to parent=${slotData.parentSlotId.blockId}"
                    ) >>
                    blockIdTree.associate(slotData.slotId.blockId, slotData.parentSlotId.blockId) >>
                    Logger[F].info(
                      show"Storing SlotData id=${slotData.slotId.blockId}"
                    ) >>
                    slotDataStore.put(slotData.slotId.blockId, slotData)
                  )
                  _ <-
                    localChain
                      .isWorseThan(tine.last)
                      .ifM(
                        // The case where the remote tine is better than the local tine,
                        // but we first need to fetch and validate all of the data
                        ifTrue = for {
                          _ <- Logger[F]
                            .info(show"Remote tine (head id=${id}) appears to be better than the local chain")
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
          }
          .toMat(Sink.ignore)(Keep.right)
          .liftFutureTo[F]
      } yield ()

    implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
      Show.fromToString

    implicit private val showTransactionSyntaxError: Show[TransactionSyntaxError] =
      Show.fromToString

    implicit private val showTransactionSemanticError: Show[TransactionSemanticError] =
      Show.fromToString

    implicit private val showBodySyntaxError: Show[BodySyntaxError] =
      Show.fromToString

    implicit private val showBodySemanticError: Show[BodySemanticError] =
      Show.fromToString

    private def fetchAndValidateMissingHeaders[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      client:           BlockchainPeerClient[F],
      headerValidation: BlockHeaderValidationAlgebra[F],
      slotDataStore:    Store[F, TypedIdentifier, SlotData],
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2]
    )(from:             TypedIdentifier) =
      determineMissingValues(
        headerStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingHeaders =>
          Source
            .fromIterator(() => missingHeaders.iterator)
            .tapAsyncF(1)(blockId => Logger[F].info(show"Fetching remote header id=$blockId"))
            .mapAsyncF(1)(blockId =>
              OptionT(client.getRemoteHeader(blockId))
                .filter(_.id.asTypedBytes === blockId)
                .getOrNoSuchElement(blockId.show)
                .tupleLeft(blockId)
            )
            .tapAsyncF(1) { case (blockId, _) =>
              Logger[F].info(show"Validating remote header id=$blockId")
            }
            .tapAsyncF(1) { case (blockId, header) =>
              EitherT(headerStore.getOrRaise(header.parentHeaderId).flatMap(headerValidation.validate(header, _)))
                .leftSemiflatTap(error => Logger[F].warn(show"Received invalid block header id=$blockId error=$error"))
                .leftMap(error => new IllegalArgumentException(error.show))
                .rethrowT >>
              Logger[F].info(show"Saving header id=$blockId") >>
              headerStore.put(blockId, header)
            }
            .toMat(Sink.ignore)(Keep.right)
            .liftFutureTo[F]
            .void
        )

    private def fetchAndValidateMissingBodies[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      client:                 BlockchainPeerClient[F],
      bodySyntaxValidation:   BodySyntaxValidationAlgebra[F],
      bodySemanticValidation: BodySemanticValidationAlgebra[F],
      slotDataStore:          Store[F, TypedIdentifier, SlotData],
      headerStore:            Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:              Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:       Store[F, TypedIdentifier, Transaction]
    )(from:                   TypedIdentifier) =
      determineMissingValues(
        bodyStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingBodyIds =>
          Source
            .fromIterator(() => missingBodyIds.iterator)
            .tapAsyncF(1)(blockId => Logger[F].info(show"Fetching remote body id=$blockId"))
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
                      Logger[F].info(show"Fetching remote transaction id=$transactionId") >>
                      OptionT(client.getRemoteTransaction(transactionId))
                        .filter(_.id.asTypedBytes === transactionId)
                        .getOrNoSuchElement(transactionId.show)
                        .flatTap(_ => Logger[F].info(show"Saving transaction id=$transactionId"))
                        .flatTap(transaction => transactionStore.put(transactionId, transaction))
                    )
                )
                .map((blockId, body, _))
            }
            .tapAsyncF(1) { case (blockId, body, _) =>
              Logger[F].info(show"Saving body id=$blockId") >>
              bodyStore.put(blockId, body)
            }
            .tapAsyncF(1) { case (blockId, body, _) =>
              headerStore
                .getOrRaise(blockId)
                .map(header => BlockV2(header, body))
                .flatMap(block =>
                  Logger[F].info(show"Validating syntax of body id=$blockId") >>
                  EitherT(bodySyntaxValidation.validate(block.blockBodyV2).map(_.toEither))
                    .leftSemiflatTap(errors =>
                      Logger[F].warn(show"Received invalid block body id=$blockId errors=$errors")
                    )
                    .leftMap(errors => new IllegalArgumentException(errors.show))
                    .semiflatTap(_ => Logger[F].info(show"Validating semantics of body id=$blockId"))
                    .flatMap(_ =>
                      EitherT(
                        bodySemanticValidation
                          .validate(block.headerV2.parentHeaderId)(block.blockBodyV2)
                          .map(_.toEither)
                      )
                        .leftSemiflatTap(errors =>
                          Logger[F].warn(show"Received invalid block body id=$blockId errors=$errors")
                        )
                        .leftMap(errors => new IllegalArgumentException(errors.show))
                    )
                    .rethrowT
                    // TODO: Delete the bad transaction, body, and header
                    .void
                )
            }
            .toMat(Sink.ignore)(Keep.right)
            .liftFutureTo[F]
        )

    private def determineMissingValues[F[_]: Monad](
      existsLocally: TypedIdentifier => F[Boolean],
      parentOf:      TypedIdentifier => F[TypedIdentifier]
    )(from:          TypedIdentifier) =
      (NonEmptyChain(from), false)
        .iterateUntilM { case (ids, _) =>
          parentOf(ids.head).flatMap(parentId =>
            existsLocally(parentId).ifM((ids, true).pure[F], (ids.prepend(parentId), false).pure[F])
          )
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
                  fetchRemoteData(parentId).map(parentData => (data.prepend((parentId, parentData)), false))
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
      transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      mempool:                     MempoolAlgebra[F]
    )(client:                      BlockchainPeerClient[F])(implicit materializer: Materializer) =
      client.remoteTransactionNotifications
        .flatMap(_.withCancel[F])
        .flatMap(source =>
          Async[F].fromFuture(
            source
              .tapAsyncF(1)(id => Logger[F].info(show"Received transaction notification from remote peer id=$id"))
              .mapAsyncF(1)(id => transactionStore.contains(id).tupleLeft(id))
              .tapAsyncF(1) {
                case (id, true) =>
                  Logger[F].info(show"Ignoring already known remote transaction id=$id")
                case (id, false) =>
                  Logger[F].info(show"Fetching remote transaction id=$id")
              }
              .collect { case (id, false) => id }
              .mapAsyncF(1)(id => OptionT(client.getRemoteTransaction(id)).getOrNoSuchElement(id.show))
              .mapAsyncF(1)(transaction =>
                EitherT(transactionSyntaxValidation.validate(transaction).map(_.toEither))
                  .semiflatTap(transaction =>
                    Logger[F].info(show"Transaction id=${transaction.id.asTypedBytes} is syntactically valid")
                  )
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
                  .info(show"Inserting transaction id=${transaction.id.asTypedBytes} into Store")
              )
              .tapAsyncF(1)(transaction => transactionStore.put(transaction.id, transaction))
              .tapAsyncF(1)(transaction =>
                Logger[F]
                  .info(
                    show"Inserting transaction id=${transaction.id.asTypedBytes} into Mempool"
                  )
              )
              .tapAsyncF(1)(transaction => mempool.add(transaction.id))
              .toMat(Sink.ignore)(Keep.right)
              .liftTo[F]
          )
        )
        .void

    private def periodicallyTraceAndLogCommonAncestor[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    )(client:                  BlockchainPeerClient[F])(implicit mat: Materializer) =
      Source
        .tick(0.seconds, 10.seconds, ())
        .withCancel[F]
        .flatMap(
          _.tapAsyncF(1)(_ => traceAndLogCommonAncestor(getLocalBlockIdAtHeight, currentHeight, slotDataStore)(client))
            .toMat(Sink.ignore)(Keep.right)
            .liftFutureTo[F]
        )
        .void

    private def traceAndLogCommonAncestor[F[_]: Sync: Logger](
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    )(client:                  BlockchainPeerClient[F]) =
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
  }
}
