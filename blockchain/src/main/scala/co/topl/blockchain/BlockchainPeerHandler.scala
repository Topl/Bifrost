package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import cats.data._
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Monoid, Parallel, Show}
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

/**
 * Provides different interpreters for `BlockchainPeerHandlerAlgebra` that can be combined and run in parallel
 * for each blockchain peer.
 */
object BlockchainPeerHandler {

  /**
   * A Monoid for `BlockchainPeerHandler` which runs the two sub-handlers in parallel.
   */
  implicit def monoidBlockchainPeerHandler[F[_]: Parallel: Applicative]: Monoid[BlockchainPeerHandlerAlgebra[F]] =
    Monoid.instance(
      (_: BlockchainPeerClient[F]) => Applicative[F].unit,
      (a, b) => (client: BlockchainPeerClient[F]) => (a.usePeer(client), b.usePeer(client)).parTupled.void
    )

  /**
   * A `BlockchainPeerClient` interpreter which listens to block IDs adopted by the remote peer.  If the remote
   * peer adopts a block that is unknown to this local node, the corresponding SlotData (and any missing ancestor SlotData)
   * is retrieved from the remote peer.  Once the SlotData is retrieved, Chain Selection is performed against the local
   * node's canonical chain.  If the remote peer claims to have a better head block, the local node fetches and validates
   * any missing Block Headers from the remote peer's chain.  Next, the local node fetches and validates any missing
   * Block Bodies from the remote peer's chain.  Finally, chain selection is run again as a redundant check in case
   * a new chain was adopted while validating the current one.  If the remote chain is _still_ better than the local
   * node's, it is adopted.
   */
  object ChainSynchronizer {

    def make[F[_]: Async: Parallel: FToFuture: RunnableGraphToF: Logger](
      localChain:             LocalChainAlgebra[F],
      headerValidation:       BlockHeaderValidationAlgebra[F],
      bodySyntaxValidation:   BodySyntaxValidationAlgebra[F],
      bodySemanticValidation: BodySemanticValidationAlgebra[F],
      slotDataStore:          Store[F, TypedIdentifier, SlotData],
      headerStore:            Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:              Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:       Store[F, TypedIdentifier, Transaction],
      blockIdTree:            ParentChildTree[F, TypedIdentifier]
    )(implicit mat:           Materializer): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
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
                    tine: NonEmptyChain[SlotData] <- buildTine[F, SlotData](
                      slotDataStore,
                      id => OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id.show),
                      (_, data) => data.parentSlotId.blockId.pure[F]
                    )(
                      (slotData.slotId.blockId, slotData)
                    )
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
                              .info(show"Remote tine (head id=$id) appears to be better than the local chain")
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

    implicit private val showBodySyntaxError: Show[BodySyntaxError] =
      Show.fromToString

    implicit private val showBodySemanticError: Show[BodySemanticError] =
      Show.fromToString

    /**
     * Fetches each missing header block from the remote peer, starting with the given `from`.  The search first determines
     * the sequence of missing header IDs by comparing IDs known in the SlotDataStore with IDs known in the HeaderStore.
     * Once the list of missing IDs is assembled, the headers are requested from the remote peer and validated in-order
     */
    private[blockchain] def fetchAndValidateMissingHeaders[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
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

    /**
     * Fetches each missing block body from the remote peer, starting with the given `from`.  The search first determines
     * the sequence of missing body IDs by comparing IDs known in the SlotDataStore with IDs known in the BodyStore.
     * Once the list of missing IDs is assembled, the bodies (and any locally missing transactions) are requested from
     * the remote peer and validated in-order.
     */
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

    /**
     * Constructs a list of IDs that are determined to be missing from store.
     * @param existsLocally a function which indicates if the given ID exists locally (i.e. headerStore.contains or bodyStore.contains)
     * @param parentOf a function which indicates the ancestor of the given ID
     * @param from the starting (head/tip) from which the search will work backwards
     */
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

    /**
     * Construct a tine of some data type `Value`.  The Tine is assembled by working backwards through the
     * chain of IDs and fetching the associated data from the remote peer
     */
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
  }

  /**
   * Interprets `BlockchainPeerHandler` by listening to all Transaction IDs announced by the remote peer.
   * Each announced transaction ID is checked locally to see if it already exists.  If not, the transaction
   * is requested from the remote peer, then syntactically validated, then inserted into the
   * local store and mempool.
   */
  object FetchMempool {

    implicit private val showTransactionSyntaxError: Show[TransactionSyntaxError] =
      Show.fromToString

    implicit private val showTransactionSemanticError: Show[TransactionSemanticError] =
      Show.fromToString

    def make[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      mempool:                     MempoolAlgebra[F]
    )(implicit materializer:       Materializer): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
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
  }

  /**
   * Interprets `BlockchainPeerHandler` by periodically running a common ancestor trace with the remote peer.  The
   * result is logged.
   */
  object CommonAncestorSearch {

    def make[F[_]: Async: FToFuture: RunnableGraphToF: Logger](
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    )(implicit mat:            Materializer): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
        Source
          .tick(0.seconds, 10.seconds, ())
          .withCancel[F]
          .flatMap(
            _.tapAsyncF(1)(_ =>
              traceAndLogCommonAncestor(getLocalBlockIdAtHeight, currentHeight, slotDataStore)(client)
            )
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

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }
}
