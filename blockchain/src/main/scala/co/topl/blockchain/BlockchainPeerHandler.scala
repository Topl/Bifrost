package co.topl.blockchain

import cats.data._
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Monoid, Parallel, Show}
import fs2._
import co.topl.algebras.ClockAlgebra.implicits.ClockOps
import co.topl.algebras.{ClockAlgebra, Store, StoreReader}
import co.topl.blockchain.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.blockchain.models.BlockHeaderToBodyValidationFailure
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.BlockHeaderValidationFailure
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.{models => legacyModels}
import co.topl.models.utility._
import legacyModels._
import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody
import co.topl.networking.blockchain._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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

    def make[F[_]: Async](
      clock:                       ClockAlgebra[F],
      localChain:                  LocalChainAlgebra[F],
      headerValidation:            BlockHeaderValidationAlgebra[F],
      headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
      bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
      bodySemanticValidation:      BodySemanticValidationAlgebra[F],
      bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
      slotDataStore:               Store[F, TypedIdentifier, SlotData],
      headerStore:                 Store[F, TypedIdentifier, BlockHeader],
      bodyStore:                   Store[F, TypedIdentifier, BlockBody],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      blockIdTree:                 ParentChildTree[F, TypedIdentifier]
    ): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
        for {
          implicit0(logger: Logger[F]) <- createPeerLogger[F](client)("Bifrost.P2P.ChainSync")
          adoptions                    <- client.remotePeerAdoptions
          _fetchAndValidateMissingHeaders = fetchAndValidateMissingHeaders(
            client,
            headerValidation,
            slotDataStore,
            headerStore
          ) _
          _fetchAndValidateMissingBodies = fetchAndValidateMissingBodies(
            client,
            headerToBodyValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            slotDataStore,
            headerStore,
            bodyStore,
            transactionStore
          ) _
          _ <- adoptions
            .evalMap(id =>
              slotDataStore
                .contains(id)
                .ifM(
                  ifTrue = Logger[F].debug(show"Ignoring already-known block header id=$id"),
                  ifFalse = for {
                    fetch <- (
                      (id: TypedIdentifier) =>
                        Logger[F].info(show"Fetching remote SlotData id=$id") >>
                        OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id)
                    ).pure[F]
                    slotData <- fetch(id)
                    // Fetch missing SlotData from the remote peer
                    tine: NonEmptyChain[SlotData] <- buildTine[F, SlotData](
                      slotDataStore,
                      fetch,
                      (_, data) => data.parentSlotId.blockId.pure[F]
                    )((slotData.slotId.blockId, slotData))
                    _ <- Logger[F].debug(show"Retrieved remote tine length=${tine.length}")
                    // We necessarily need to save Slot Data in the store prior to performing chain "preference"
                    _ <- tine.traverse(slotData =>
                      Logger[F].debug(
                        show"Associating child=${slotData.slotId.blockId} to parent=${slotData.parentSlotId.blockId}"
                      ) >>
                      blockIdTree.associate(slotData.slotId.blockId, slotData.parentSlotId.blockId) >>
                      Logger[F].debug(show"Storing SlotData id=${slotData.slotId.blockId}") >>
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
                              .debug(show"Remote tine (head id=$id) appears to be better than the local chain")
                            // The remote tine may be large, extending beyond 2 epochs.  Header validation relies on
                            // block bodies (lagging by 2 epochs), so group the tine by epoch step through each
                            // group.
                            epochBoundaries <- tine
                              .foldLeftM(Chain.empty[SlotId]) { case (boundaries, slotData) =>
                                boundaries.initLast.fold(Chain(slotData.slotId).pure[F]) { case (init, last) =>
                                  (clock.epochOf(slotData.slotId.slot), clock.epochOf(last.slot))
                                    .mapN((epoch, parentEpoch) =>
                                      if (epoch > parentEpoch) boundaries.append(slotData.slotId)
                                      else init.append(slotData.slotId)
                                    )
                                }
                              }
                              .map(_.map(_.blockId))
                            _ <- epochBoundaries
                              .traverseTap(id =>
                                _fetchAndValidateMissingHeaders(id) >> _fetchAndValidateMissingBodies(id)
                              )
                            _ <-
                              // After fetching and validating all of the data, re-run the chain preference process
                              localChain
                                .isWorseThan(tine.last)
                                .ifM(
                                  ifTrue =
                                    // And finally, adopt the remote peer's tine
                                    localChain.adopt(Validated.Valid(tine.last)),
                                  ifFalse = Logger[F].debug(show"Ignoring weaker (or equal) block header id=$id")
                                )
                          } yield (),
                          // The case where the remote tine can be ignored
                          Logger[F].info(show"Ignoring weaker (or equal) block header id=$id")
                        )
                  } yield ()
                )
            )
            .compile
            .drain
        } yield ()

    implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
      Show.fromToString

    implicit private val showBodySyntaxError: Show[BodySyntaxError] =
      Show.fromToString

    implicit private val showBodySemanticError: Show[BodySemanticError] =
      Show.fromToString

    implicit private val showBodyAuthorizationError: Show[BodyAuthorizationError] =
      Show.fromToString

    implicit private val showHeaderToBodyError: Show[BlockHeaderToBodyValidationFailure] =
      Show.fromToString

    /**
     * Fetches each missing header block from the remote peer, starting with the given `from`.  The search first determines
     * the sequence of missing header IDs by comparing IDs known in the SlotDataStore with IDs known in the HeaderStore.
     * Once the list of missing IDs is assembled, the headers are requested from the remote peer and validated in-order
     */
    private[blockchain] def fetchAndValidateMissingHeaders[F[_]: Async: Logger](
      client:           BlockchainPeerClient[F],
      headerValidation: BlockHeaderValidationAlgebra[F],
      slotDataStore:    Store[F, TypedIdentifier, SlotData],
      headerStore:      Store[F, TypedIdentifier, BlockHeader]
    )(from:             TypedIdentifier) =
      determineMissingValues(
        headerStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingHeaders =>
          Stream
            .foldable[F, NonEmptyChain, TypedIdentifier](missingHeaders)
            .parEvalMapUnbounded(blockId =>
              for {
                _      <- Logger[F].info(show"Fetching remote header id=$blockId")
                header <- OptionT(client.getRemoteHeader(blockId)).getOrNoSuchElement(blockId.show)
                _ <- MonadThrow[F].raiseWhen(header.id.asTypedBytes =!= blockId)(
                  new IllegalArgumentException("Claimed block ID did not match provided header")
                )
              } yield (blockId, header)
            )
            .evalMap { case (blockId, header) =>
              for {
                _ <- Logger[F].debug(show"Validating remote header id=$blockId")
                _ <- EitherT(
                  headerStore
                    .getOrRaise(header.parentHeaderId)
                    .flatMap(headerValidation.validate(header, _))
                )
                  .leftSemiflatTap(error =>
                    Logger[F].warn(show"Received invalid block header id=$blockId error=$error")
                  )
                  .leftMap(error => new IllegalArgumentException(error.show))
                  .rethrowT
                _ <- Logger[F].debug(show"Saving header id=$blockId")
                _ <- headerStore.put(blockId, header)
              } yield ()
            }
            .compile
            .drain
        )

    /**
     * Fetches each missing block body from the remote peer, starting with the given `from`.  The search first determines
     * the sequence of missing body IDs by comparing IDs known in the SlotDataStore with IDs known in the BodyStore.
     * Once the list of missing IDs is assembled, the bodies (and any locally missing transactions) are requested from
     * the remote peer and validated in-order.
     */
    private def fetchAndValidateMissingBodies[F[_]: Async: Logger](
      client:                      BlockchainPeerClient[F],
      headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
      bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
      bodySemanticValidation:      BodySemanticValidationAlgebra[F],
      bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
      slotDataStore:               Store[F, TypedIdentifier, SlotData],
      headerStore:                 Store[F, TypedIdentifier, BlockHeader],
      bodyStore:                   Store[F, TypedIdentifier, BlockBody],
      transactionStore:            Store[F, TypedIdentifier, Transaction]
    )(from:                        TypedIdentifier) =
      determineMissingValues(
        bodyStore.contains,
        slotDataStore.getOrRaise(_).map(_.parentSlotId.blockId)
      )(from)
        .flatMap(missingBodyIds =>
          Stream
            .foldable(missingBodyIds)
            .evalMap(blockId =>
              for {
                _    <- Logger[F].info(show"Fetching remote body id=$blockId")
                body <- OptionT(client.getRemoteBody(blockId)).getOrNoSuchElement(blockId.show)
                _ <- Stream
                  .iterable(body.transactionIds)
                  .parEvalMapUnorderedUnbounded(transactionId =>
                    transactionStore
                      .contains(transactionId)
                      .ifM(
                        Applicative[F].unit,
                        for {
                          _ <- Logger[F].info(show"Fetching remote transaction id=$transactionId")
                          transaction <- OptionT(client.getRemoteTransaction(transactionId))
                            .getOrNoSuchElement(transactionId.show)
                          _ <- MonadThrow[F]
                            .raiseWhen(transaction.id.asTypedBytes =!= transactionId)(
                              new IllegalArgumentException("Claimed transaction ID did not match provided transaction")
                            )
                          _ <- Logger[F].debug(show"Saving transaction id=$transactionId")
                          _ <- transactionStore.put(transactionId, transaction)
                        } yield ()
                      )
                  )
                  .compile
                  .drain
                header <- headerStore.getOrRaise(blockId)
                block = Block(header, body)
                _ <-
                  (
                    for {
                      _ <- EitherT.liftF(Logger[F].debug(show"Validating header to body consistency for id=$blockId"))
                      _ <- EitherT(headerToBodyValidation.validate(block)).leftMap(e => e.show)
                      _ <- EitherT.liftF(Logger[F].debug(show"Validating syntax of body id=$blockId"))
                      _ <- EitherT(bodySyntaxValidation.validate(body).map(_.toEither.leftMap(_.show)))
                      _ <- EitherT.liftF(Logger[F].debug(show"Validating semantics of body id=$blockId"))
                      validationContext = StaticBodyValidationContext(header.parentHeaderId, header.height, header.slot)
                      _ <- EitherT(
                        bodySemanticValidation.validate(validationContext)(body).map(_.toEither.leftMap(_.show))
                      )
                      _ <- EitherT.liftF(Logger[F].debug(show"Validating authorization of body id=$blockId"))
                      _ <- EitherT(
                        bodyAuthorizationValidation
                          .validate(header.parentHeaderId)(body)
                          .map(_.toEither.leftMap(_.show))
                      )
                    } yield ()
                  )
                    .leftSemiflatTap(e => Logger[F].warn(show"Received invalid block body id=$blockId errors=$e"))
                    .leftMap((e: String) => new IllegalArgumentException(e))
                    .rethrowT
                _ <- Logger[F].debug(show"Saving body id=$blockId")
                _ <- bodyStore.put(blockId, body)
              } yield ()
            )
            .compile
            .drain
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
            existsLocally(parentId).ifM(ifTrue = (ids, true).pure[F], ifFalse = (ids.prepend(parentId), false).pure[F])
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
    )(from:            (TypedIdentifier, Value)): F[NonEmptyChain[Value]] =
      // Tuple: (Data Elements, Parent Exists Locally)
      // Starting with `from`, work backwards until a local value is found
      (NonEmptyChain(from), false)
        .iterateUntilM { case (data, _) =>
          // First determine the parent of the head of the current list of data elements
          parentOf(data.head._1, data.head._2)
            .flatMap(parentId =>
              // Now check to see if that parent exists locally
              store
                .contains(parentId)
                .ifM(
                  // If it exists locally, we're all done
                  ifTrue = (data, true).pure[F],
                  // Otherwise, prepend it to the data elements and re-iterate
                  ifFalse = fetchRemoteData(parentId).map(parentData => (data.prepend((parentId, parentData)), false))
                )
            )
        // Stop iterating once a local value has been found
        }(_._2)
        // Keep just the data elements
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

    def make[F[_]: Async](
      transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      mempool:                     MempoolAlgebra[F]
    ): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
        createPeerLogger[F](client)("Bifrost.P2P.MempoolSync").flatMap(implicit logger =>
          client.remoteTransactionNotifications
            .flatMap(source =>
              source
                .parEvalMapUnordered(16)(
                  processTransactionId[F](transactionSyntaxValidation, transactionStore, mempool)(client)
                )
                .compile
                .drain
            )
            .void
        )

    private def processTransactionId[F[_]: Async: Logger](
      transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
      transactionStore:            Store[F, TypedIdentifier, Transaction],
      mempool:                     MempoolAlgebra[F]
    )(client:                      BlockchainPeerClient[F])(id: TypedIdentifier) =
      for {
        _             <- Logger[F].debug(show"Received transaction notification from remote peer id=$id")
        alreadyExists <- transactionStore.contains(id)
        _ <-
          if (alreadyExists)
            Logger[F].debug(show"Ignoring already known remote transaction id=$id")
          else {
            for {
              _           <- Logger[F].info(show"Fetching remote transaction id=$id")
              transaction <- OptionT(client.getRemoteTransaction(id)).getOrNoSuchElement(id.show)
              _ <- MonadThrow[F].raiseWhen(transaction.id.asTypedBytes =!= id)(
                new IllegalArgumentException(s"Claimed transaction ID did not match provided transaction")
              )
              _ <- EitherT(transactionSyntaxValidation.validate(transaction).map(_.toEither))
                .leftSemiflatMap(errors =>
                  Logger[F].warn(show"Received syntactically invalid transaction id=$id errors=$errors")
                )
                .leftMap(errors => new IllegalArgumentException(errors.show))
                .rethrowT
              _ <- Logger[F].debug(show"Transaction id=$id is syntactically valid")
              _ <- Logger[F].debug(show"Inserting transaction id=$id into Mempool and Store")
              _ <- transactionStore.put(id, transaction)
              _ <- mempool.add(id)
            } yield ()
          }
      } yield ()
  }

  /**
   * Interprets `BlockchainPeerHandler` by periodically running a common ancestor trace with the remote peer.  The
   * result is logged.
   */
  object CommonAncestorSearch {

    def make[F[_]: Async](
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    ): BlockchainPeerHandlerAlgebra[F] =
      (client: BlockchainPeerClient[F]) =>
        createPeerLogger[F](client)("Bifrost.P2P.CommonAncestorTrace")
          .flatMap(implicit logger =>
            Stream
              .fixedDelay[F](10.seconds)
              .evalMap(_ => traceAndLogCommonAncestor(getLocalBlockIdAtHeight, currentHeight, slotDataStore)(client))
              .compile
              .drain
          )

    private def traceAndLogCommonAncestor[F[_]: Sync: Logger](
      getLocalBlockIdAtHeight: Long => F[TypedIdentifier],
      currentHeight:           () => F[Long],
      slotDataStore:           StoreReader[F, TypedIdentifier, SlotData]
    )(client:                  BlockchainPeerClient[F]) =
      Sync[F]
        .defer(
          Logger[F].debug(show"Starting common ancestor trace") >>
          client
            .findCommonAncestor(getLocalBlockIdAtHeight, currentHeight)
            .flatTap(ancestor =>
              OptionT(slotDataStore.get(ancestor))
                .getOrNoSuchElement(ancestor)
                .flatMap(slotData =>
                  Logger[F].info(
                    show"Traced common ancestor to" +
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

  implicit class OptionTOps[F[_], T](val optionT: OptionT[F, T]) extends AnyVal {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  private def createPeerLogger[F[_]: Sync](client: BlockchainPeerClient[F])(processName: String) =
    client.remotePeer.map(remotePeer =>
      Slf4jLogger
        .getLoggerFromName[F](show"$processName [${remotePeer.remoteAddress}]")
    )

}
