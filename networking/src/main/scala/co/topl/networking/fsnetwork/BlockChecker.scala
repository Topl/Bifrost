package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data._
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.ledger.algebras._
import co.topl.ledger.interpreters.QuivrContext
import co.topl.ledger.models.{BodyValidationError, StaticBodyValidationContext}
import co.topl.networking.fsnetwork.BlockChecker.Message._
import co.topl.networking.fsnetwork.BlockApplyError.BodyApplyException.BodyValidationException
import co.topl.networking.fsnetwork.BlockApplyError.{BodyApplyException, HeaderApplyException}
import co.topl.networking.fsnetwork.BlockApplyError.HeaderApplyException.HeaderValidationException
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

/**
 * TODO consider to split to two separate actors
 */
object BlockChecker {
  sealed trait Message

  object Message {

    /**
     * Process new slot data from remote peer, if incoming slot data is better than from any other host then start to
     * trying to adopt data from that peer
     * @param source source of slot data, used as a hint from which peer headers shall be requested
     * @param slotData slot data to compare, slot data chain contains only new remote slot data.
     */
    case class RemoteSlotData(source: HostId, slotData: NonEmptyChain[SlotData]) extends Message

    /**
     * Check and adopt remote headers, if headers is valid then appropriate bodies will be requested
     * @param source source of headers, used as a hint from which peer bodies shall be requested
     * @param idsAndHeaders headers to check and adopt
     */
    case class RemoteBlockHeaders(source: HostId, idsAndHeaders: NonEmptyChain[(BlockId, BlockHeader)]) extends Message

    /**
     * check and adopt block bodies, if adopted bodies is better than local chain
     * then remote bodies became new top block
     * @param source source of bodies, could be used as a hint from which peer next bodies shall be requested
     * @param idsAndBodies bodies to check
     */
    case class RemoteBlockBodies(source: HostId, idsAndBodies: NonEmptyChain[(BlockId, BlockBody)]) extends Message

    /**
     * Invalidate block because block is invalid by some reason, for example no block body available at any peer or
     * validation of block had been failed
     * @param blockId invalid blockId
     */
    case class InvalidateBlockId(blockId: BlockId) extends Message
  }

  case class State[F[_]](
    reputationAggregator:        ReputationAggregatorActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    chainSelection:              ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    // TODO maybe use some kind of stack to not loose previous best slot data; TODO use more efficient structure
    bestKnownRemoteSlotDataOpt: Option[BestChain],
    // TODO will be deleted hostId shall be selected by peers manager
    bestKnownRemoteSlotDataHost: Option[HostId]
  )

  type Response[F[_]] = State[F]
  type BlockCheckerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, RemoteSlotData(hostId, slotData))         => processSlotData(state, hostId, slotData)
      case (state, RemoteBlockHeaders(hostId, blockHeaders)) => processRemoteHeaders(state, hostId, blockHeaders)
      case (state, RemoteBlockBodies(hostId, blockBodies))   => processRemoteBodies(state, hostId, blockBodies)
      case (state, InvalidateBlockId(invalidBlockId))        => processInvalidBlockId(state, invalidBlockId)
    }

  def makeActor[F[_]: Async: Logger](
    reputationAggregator:        ReputationAggregatorActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    bestChain:                   Option[BestChain] = None,
    bestKnownRemoteSlotDataHost: Option[HostId] = None
  ): Resource[F, BlockCheckerActor[F]] = {
    val initialState =
      State(
        reputationAggregator,
        requestsProxy,
        localChain,
        slotDataStore,
        headerStore,
        bodyStore,
        chainSelectionAlgebra,
        headerValidation,
        bodySyntaxValidation,
        bodySemanticValidation,
        bodyAuthorizationValidation,
        bestChain,
        bestKnownRemoteSlotDataHost
      )
    Actor.make(initialState, getFsm[F])
  }

  private def processSlotData[F[_]: Async: Logger](
    state:           State[F],
    candidateHostId: HostId,
    slotData:        NonEmptyChain[SlotData]
  ): F[(State[F], Response[F])] = {
    val bestRemoteSlotData: SlotData = slotData.last
    val bestRemoteBlockId: BlockId = bestRemoteSlotData.slotId.blockId

    for {
      _ <- Logger[F].info(show"Received slot data proposal with best block id $bestRemoteBlockId")
      newState <- remoteSlotDataBetter(state, bestRemoteSlotData).ifM(
        ifTrue = processNewBestSlotData(state, slotData, candidateHostId),
        ifFalse = Logger[F].debug(s"Ignore weaker slot data $bestRemoteBlockId") >> state.pure[F]
      )
    } yield (newState, newState)
  }

  private def remoteSlotDataBetter[F[_]: Async](state: State[F], bestRemoteSlotData: SlotData): F[Boolean] = {
    val localBestSlotData = state.bestKnownRemoteSlotDataOpt.map(_.last.pure[F]).getOrElse(state.localChain.head)
    localBestSlotData.flatMap(local => state.chainSelection.compare(bestRemoteSlotData, local).map(_ > 0))
  }

  private def processNewBestSlotData[F[_]: Async: Logger](
    state:           State[F],
    remoteSlotData:  NonEmptyChain[SlotData],
    candidateHostId: HostId
  ): F[State[F]] = {
    val remoteIds: NonEmptyChain[BlockId] = remoteSlotData.map(_.slotId.blockId)
    for {
      fullSlotData <- buildFullSlotDataChain(state, remoteSlotData)
      _            <- Logger[F].debug(show"Extend slot data $remoteIds to ${fullSlotData.map(_.slotId.blockId)}")
      newState     <- changeLocalSlotData(state, fullSlotData, candidateHostId)
      _            <- requestNextHeaders(newState)
    } yield newState
  }

  /**
   * Received remote slot data could be not full, because some of slot data could be already saved in slot storage,
   * thus we build full slot data
   * @param state actor state
   * @param remoteSlotData incomplete store data
   * @tparam F effect
   * @return if we return chain of slot data A0 -> A1 -> ... -> AN, then:
   *         AN == slotData.last
   *         A0 - AN == slot data for appropriate block A0-AN and header for block A0-AN is absent in header storage
   *         Header storage shall contain block header which is parent of block A0
   */
  private def buildFullSlotDataChain[F[_]: Async](
    state:          State[F],
    remoteSlotData: NonEmptyChain[SlotData]
  ): F[NonEmptyChain[SlotData]] = {
    val from = remoteSlotData.head.parentSlotId.blockId
    val missedSlotDataF = getFromChainUntil[F, SlotData](
      getSlotDataFromT = s => s.pure[F],
      getT = state.slotDataStore.getOrRaise,
      terminateOn = sd => state.headerStore.contains(sd.slotId.blockId)
    )(from)

    missedSlotDataF.map(sd => remoteSlotData.prependChain(Chain.fromSeq(sd)))
  }

  private def changeLocalSlotData[F[_]: Async: Logger](
    state:       State[F],
    newSlotData: NonEmptyChain[SlotData],
    hostId:      HostId
  ): F[State[F]] =
    Logger[F].info(show"Update best local slot data with ${newSlotData.last.slotId.blockId}") >>
    state
      .copy(bestKnownRemoteSlotDataOpt = Option(BestChain(newSlotData)), bestKnownRemoteSlotDataHost = Option(hostId))
      .pure[F]

  private def requestNextHeaders[F[_]: MonadThrow: Logger](state: State[F]): F[Unit] =
    state.bestKnownRemoteSlotDataOpt
      .map { slotData =>
        getFirstNMissedInStore(state.headerStore, state.slotDataStore, slotData.lastId, chunkSize)
          .flatTap(m => OptionT.liftF(Logger[F].info(show"Send request to get missed headers for blockIds: $m")))
          .map(RequestsProxy.Message.DownloadHeadersRequest(state.bestKnownRemoteSlotDataHost.get, _))
          .foreachF(state.requestsProxy.sendNoWait)
      }
      .getOrElse(().pure[F])
      .handleErrorWith(e => Logger[F].error(show"Failed to request next headers due ${e.toString}"))

  private def processRemoteHeaders[F[_]: Async: Logger](
    state:        State[F],
    hostId:       HostId,
    blockHeaders: NonEmptyChain[(BlockId, BlockHeader)]
  ): F[(State[F], Response[F])] = {
    val processedHeadersAndErrors =
      Stream
        .foldable(blockHeaders)
        .covaryAll[F, (BlockId, BlockHeader)]
        .evalDropWhile(knownBlockHeaderPredicate(state))
        .evalMap(verifyOneBlockHeader(state))
        .evalTap { case (id, header) => state.headerStore.put(id, header) }
        .map { case (id, _) => Right(id) }
        .handleErrorWith {
          case e: HeaderValidationException => Stream.emit(Left(e: HeaderApplyException))
          case e                            => Stream.emit(Left(HeaderApplyException.UnknownError(e)))
        }
        .evalTap(res => logHeaderValidationResult(res))
        .compile
        .toList
        .map { list =>
          val successfullyProcessed = list.collect { case Right(d) => d }
          val error = list.collectFirst { case Left(e: HeaderApplyException) => e }
          (successfullyProcessed, error)
        }

    for {
      (appliedBlockIds, error) <- processedHeadersAndErrors
      newState                 <- processHeaderValidationError(state, error)
      _                        <- requestMissedBodiesForKnownHeaders(newState, hostId, appliedBlockIds.lastOption)
      _                        <- requestNextHeaders(newState)
    } yield (newState, newState)
  }

  private def knownBlockHeaderPredicate[F[_]: Async: Logger](
    state: State[F]
  ): ((BlockId, BlockHeader)) => F[Boolean] = { case (id, _) =>
    state.headerStore.contains(id).flatTap {
      case true  => Logger[F].info(show"Ignore know block header id $id")
      case false => Logger[F].info(show"Start processing new header $id")
    }
  }

  private def verifyOneBlockHeader[F[_]: Async: Logger](state: State[F])(idAndHeader: (BlockId, BlockHeader)) = {
    val (id, header) = idAndHeader
    Logger[F].debug(show"Validating remote header id=$id") >>
    EitherT(state.headerValidation.validate(header)).bimap(HeaderValidationException(id, _), (id, _)).rethrowT
  }

  private def logHeaderValidationResult[F[_]: Logger](res: Either[HeaderApplyException, BlockId]) =
    res match {
      case Right(id) =>
        Logger[F].info(show"Successfully process header: $id")
      case Left(HeaderValidationException(id, error)) =>
        Logger[F].error(show"Failed to apply header $id due validation error: $error")
      case Left(HeaderApplyException.UnknownError(error)) =>
        Logger[F].error(show"Failed to apply header due next error: ${error.toString}")
    }

  private def processHeaderValidationError[F[_]: Async: Logger](
    state: State[F],
    error: Option[HeaderApplyException]
  ): F[State[F]] =
    error
      .map {
        case e: HeaderValidationException => invalidateBlockId(state, e.blockId)
        case _                            => state.pure[F] // TODO any error message for underlying exception?
      }
      .getOrElse(state.pure[F])

  private def requestMissedBodiesForKnownHeaders[F[_]: Async: Logger](
    state:           State[F],
    hostId:          HostId,
    bestKnownTipOpt: Option[BlockId]
  ): F[Unit] = {
    def takeWithKnownHeaders(ids: NonEmptyChain[BlockId]) =
      OptionT(
        fs2.Stream
          .emits(ids.toList)
          .evalMap(id => state.headerStore.get(id).map((id, _)))
          .takeWhile { case (_, headerOpt) => headerOpt.isDefined }
          .map { case (id, headerOpt) => (id, headerOpt.get) }
          .compile
          .toList
          .map(NonEmptyChain.fromSeq)
      )

    val requestMissedBodiesCommand =
      for {
        bestKnownTip  <- OptionT.fromOption[F](bestKnownTipOpt)
        unknownBodies <- getFirstNMissedInStore(state.bodyStore, state.slotDataStore, bestKnownTip, chunkSize)
        unknownBodiesWithHeaders <- takeWithKnownHeaders(unknownBodies)
        _ <- OptionT.liftF(Logger[F].info(show"Send request to get bodies for: ${unknownBodiesWithHeaders.map(_._1)}"))
        message = RequestsProxy.Message.DownloadBodiesRequest(hostId, unknownBodiesWithHeaders)
        _ <- OptionT.liftF(state.requestsProxy.sendNoWait(message))
      } yield ()

    requestMissedBodiesCommand
      .getOrElse(().pure[F])
      .void
      .handleErrorWith(e => Logger[F].error(show"Failed to request next bodies for known headers due ${e.toString}"))
  }

  private def processRemoteBodies[F[_]: Async: Logger](
    state:       State[F],
    hostId:      HostId,
    blockBodies: NonEmptyChain[(BlockId, BlockBody)]
  ): F[(State[F], Response[F])] = {
    val processedBlocksAndError =
      Stream
        .foldable(blockBodies)
        .covaryAll[F, (BlockId, BlockBody)]
        .evalDropWhile(knownBlockBodyPredicate(state))
        .evalMap(verifyOneBlockBody(state))
        .evalTap { case (id, block) => state.bodyStore.put(id, block.body) }
        .evalTap(applyOneBlockBody(state))
        .map { case (id, _) => Right(id) }
        .handleErrorWith {
          case e: BodyValidationException => Stream.emit(Left(e: BodyApplyException))
          case e                          => Stream.emit(Left(BodyApplyException.UnknownError(e)))
        }
        .evalTap(res => logBodyValidationResult(res))
        .compile
        .toList
        .map { list =>
          val successfullyProcessed = list.collect { case Right(d) => d }
          val error = list.collectFirst { case Left(e: BodyApplyException) => e }
          (successfullyProcessed, error)
        }

    for {
      (appliedBlockIds, error) <- processedBlocksAndError
      stateAfterError          <- processBodyValidationError(state, error)
      _                        <- requestNextBodies(stateAfterError, hostId)
      newState                 <- updateState(stateAfterError, appliedBlockIds.lastOption).pure[F]
    } yield (newState, newState)
  }

  private def knownBlockBodyPredicate[F[_]: Async: Logger](state: State[F]): ((BlockId, BlockBody)) => F[Boolean] = {
    case (id, _) =>
      state.bodyStore.contains(id).flatTap {
        case true  => Logger[F].info(show"Ignore know block body id $id")
        case false => Logger[F].info(show"Start processing new block $id")
      }
  }

  private def verifyOneBlockBody[F[_]: Async: Logger](state: State[F])(idAndBody: (BlockId, BlockBody)) = {
    val (id, body) = idAndBody
    for {
      _     <- Logger[F].info(show"For block: $id try to get header for body")
      block <- state.headerStore.getOrRaise(id).map(header => Block(header, body))
      _     <- verifyBlockBody(state, id, block).leftMap(BodyValidationException(id, _)).rethrowT
    } yield (id, block)
  }

  private def applyOneBlockBody[F[_]: Async: Logger](state: State[F])(idAndBody: (BlockId, Block)): F[Unit] = {
    val (id, _) = idAndBody
    for {
      lastBlockSlotData <- state.slotDataStore.getOrRaise(id)
      _ <- state.localChain
        .isWorseThan(lastBlockSlotData)
        .ifM(
          ifTrue = state.localChain.adopt(Validated.Valid(lastBlockSlotData)) >>
            Logger[F].info(show"Successfully adopted block: $id"),
          ifFalse = Logger[F].info(show"Ignoring weaker (or equal) block header id=$id")
        )
    } yield ()
  }

  private def logBodyValidationResult[F[_]: Logger](res: Either[BodyApplyException, BlockId]) =
    res match {
      case Right(id) =>
        Logger[F].info(show"Successfully process body: $id")
      case Left(BodyValidationException(id, errors)) =>
        Logger[F].error(show"Failed to apply body $id due validation error: ${errors.mkString_(",")}")
      case Left(BodyApplyException.UnknownError(error)) =>
        Logger[F].error(show"Failed to apply body due next error: ${error.toString}")
    }

  private def processBodyValidationError[F[_]: Async: Logger](
    state: State[F],
    error: Option[BodyApplyException]
  ): F[State[F]] =
    error
      .map {
        case e: BodyValidationException => invalidateBlockId(state, e.blockId)
        case _                          => state.pure[F] // TODO any error message for underlying exception?
      }
      .getOrElse(state.pure[F])

  private def verifyBlockBody[F[_]: Async: Logger](
    state:   State[F],
    blockId: BlockId,
    block:   Block
  ): EitherT[F, NonEmptyChain[BodyValidationError], (BlockId, Block)] = {
    val header = block.header
    val body = block.body

    for {
      _ <- EitherT.liftF(Logger[F].debug(show"Validating syntax of body id=$blockId"))
      _ <- EitherT(state.bodySyntaxValidation.validate(body).map(_.toEither))
      _ <- EitherT.liftF(Logger[F].debug(show"Validating semantics of body id=$blockId"))
      validationContext = StaticBodyValidationContext(header.parentHeaderId, header.height, header.slot)
      _ <- EitherT(state.bodySemanticValidation.validate(validationContext)(body).map(_.toEither))
      _ <- EitherT.liftF(Logger[F].debug(show"Validating authorization of body id=$blockId"))
      authValidation = state.bodyAuthorizationValidation.validate(QuivrContext.forConstructedBlock(header, _))(body)
      _ <- EitherT(authValidation.map(_.toEither.leftMap(e => e: NonEmptyChain[BodyValidationError])))
    } yield (blockId, block)
  }

  private def requestNextBodies[F[_]: Async: Logger](state: State[F], hostId: HostId): F[Unit] =
    requestMissedBodiesForKnownHeaders(state, hostId, state.bestKnownRemoteSlotDataOpt.map(_.lastId))

  // clear bestKnownRemoteSlotData at the end of sync, so new slot data will be compared with local chain again
  private def updateState[F[_]](state: State[F], newTopBlockOpt: Option[BlockId]): State[F] = {
    for {
      bestChain   <- state.bestKnownRemoteSlotDataOpt
      newTopBlock <- newTopBlockOpt
    } yield
      if (bestChain.isLastId(newTopBlock))
        state.copy(bestKnownRemoteSlotDataOpt = None, bestKnownRemoteSlotDataHost = None)
      else
        state
  }.getOrElse(state)

  private def processInvalidBlockId[F[_]: Async: Logger](
    state:          State[F],
    invalidBlockId: BlockId
  ): F[(State[F], Response[F])] =
    invalidateBlockId(state, invalidBlockId).map(s => (s, s))

  private def invalidateBlockId[F[_]: Async: Logger](
    state:          State[F],
    invalidBlockId: BlockId
  ): F[State[F]] = {
    val invalidBlockOnCurrentBestChain =
      state.bestKnownRemoteSlotDataOpt.exists(_.containsBlockId(invalidBlockId))

    if (invalidBlockOnCurrentBestChain) {
      val newState = state.copy(bestKnownRemoteSlotDataOpt = None, bestKnownRemoteSlotDataHost = None)
      Logger[F].error("clean current best chain due error in validation") >>
      newState.pure[F]
    } else {
      state.pure[F]
    }
  }
}
