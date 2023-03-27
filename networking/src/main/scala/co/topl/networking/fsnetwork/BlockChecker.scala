package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data._
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.ledger.algebras._
import co.topl.ledger.interpreters.QuivrContext
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.networking.fsnetwork.BlockChecker.Message._
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
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
     * @param headers headers to check and adopt
     */
    case class RemoteBlockHeader(source: HostId, headers: NonEmptyChain[(BlockId, BlockHeader)]) extends Message

    /**
     * check and adopt block bodies, if adopted bodies is better than local chain then remote bodies became new top block
     * @param source source of bodies, could be used as a hint from which peer next bodies shall be requested
     * @param bodies bodies to check
     */
    case class RemoteBlockBodies(source: HostId, bodies: NonEmptyChain[(BlockId, BlockBody)]) extends Message
    // TODO implement it, in case if verification of some incoming data fot current best slot is failed
    //  then we need to reset current best slot data (and request it again) to not stuck on incorrect slot data
    // case class ClearSlotData() extends Message
  }

  case class State[F[_]](
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    chainSelection:              ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
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
      case (state, RemoteSlotData(hostId, slotData))        => processSlotData(state, hostId, slotData)
      case (state, RemoteBlockHeader(hostId, blockHeaders)) => processRemoteHeaders(state, hostId, blockHeaders)
      case (state, RemoteBlockBodies(_, blockBodies))       => processRemoteBodies(state, blockBodies)
    }

  def makeActor[F[_]: Async: Logger](
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
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
        peersManager,
        requestsProxy,
        localChain,
        slotDataStore,
        headerStore,
        bodyStore,
        chainSelectionAlgebra,
        headerValidation,
        headerToBodyValidation,
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
    val bestRemoteSlotData = slotData.last
    val bestRemoteBlockId = bestRemoteSlotData.slotId.blockId

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
  ): F[State[F]] =
    for {
      fullSlotData <- buildFullSlotDataChain(state, remoteSlotData)
      fullSlotDataIds = fullSlotData.map(_.slotId.blockId)
      _        <- Logger[F].debug(show"Extend slot data ${remoteSlotData.map(_.slotId.blockId)} to $fullSlotDataIds")
      newState <- changeLocalSlotData(state, fullSlotData, candidateHostId)
      isChainExtension = state.bestKnownRemoteSlotDataOpt.exists(_.isExtendedBy(remoteSlotData))
      // we do NOT ask new headers if new slot data is overlap of current best chain
      _ <- if (isChainExtension) Logger[F].info("Receive extension for current chain") else requestNextHeaders(newState)
    } yield newState

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
        getFirstNMissedInStore(state.headerStore, state.slotDataStore, slotData.last, chunkSize)
          .flatTap(m => OptionT.liftF(Logger[F].info(show"Send request to get missed headers for blockIds: $m")))
          .map(PeersManager.Message.BlockHeadersRequest(state.bestKnownRemoteSlotDataHost.get, _))
          .foreachF(state.peersManager.sendNoWait)
      }
      .getOrElse(().pure[F])

  private def processRemoteHeaders[F[_]: Async: Logger](
    state:        State[F],
    hostId:       HostId,
    blockHeaders: NonEmptyChain[(BlockId, BlockHeader)]
  ): F[(State[F], Response[F])] = {
    val processResult =
      for {
        _ <- EitherT.right(Logger[F].info(show"Received headers: ${blockHeaders.map(_._1)}"))
        newHeadersOpt = state.headerStore.filterKnownBlocks(blockHeaders)
        newHeaders       <- EitherT.fromOptionF(newHeadersOpt, "Skip validation for known headers")
        (verifiedIds, _) <- verifyAndSaveHeaders(state, newHeaders)
        _                <- EitherT.right[String](requestBlockBodies(state, hostId, verifiedIds.last))
        // we no need to request next headers if we are no longer on the best chain
        receivedHeadersAreInBestChain = state.bestKnownRemoteSlotDataOpt.exists(_.contains(verifiedIds.last))
        _ <- EitherT.right[String](if (receivedHeadersAreInBestChain) requestNextHeaders(state) else ().pure[F])
      } yield verifiedIds

    processResult
      .biSemiflatTap(
        error => Logger[F].info(show"Failed to apply headers due: $error"),
        newHeadersIds => Logger[F].info(show"Successfully adopted headers: $newHeadersIds")
      )
      .value as (state, state)
  }

  private def verifyAndSaveHeaders[F[_]: Async: Logger](
    state:        State[F],
    blockHeaders: NonEmptyChain[(BlockId, BlockHeader)]
  ): EitherT[F, HostId, (NonEmptyChain[BlockId], NonEmptyChain[BlockHeader])] = {
    def verifyAndSaveHeader(headerId: BlockId, header: BlockHeader) =
      for {
        _ <- Logger[F].debug(show"Validating remote header id=$headerId")
        _ <- EitherT(state.headerValidation.validate(header))
          .leftSemiflatTap(error => Logger[F].warn(show"Received invalid block header id=$headerId error=$error"))
          .leftMap(error => new IllegalArgumentException(error.show))
          .rethrowT
        _ <- Logger[F].info(show"Saving header id=$headerId")
        _ <- state.headerStore.put(headerId, header)
      } yield (headerId, header)

    EitherT
      .fromOptionF(
        Stream
          .foldable(blockHeaders)
          .evalMap { case (headerId, header) => verifyAndSaveHeader(headerId, header) }
          .compile
          .toList
          .handleError(_ => List.empty[(BlockId, BlockHeader)]) // @TODO return actual list of applied bodies
          .map(b => NonEmptyChain.fromSeq(b)),
        "No verified blocks headers had been found or some block headers are not correct"
      )
      .map(_.unzip)
  }

  private def requestBlockBodies[F[_]: Async: Logger](
    state:             State[F],
    hostId:            HostId,
    bestKnownHeaderId: BlockId
  ): F[Unit] = {
    val getMissedBodiesFun =
      getFromChainUntil[F, BlockId](state.slotDataStore.getOrRaise, id => id.pure[F], state.bodyStore.contains) _

    val requestMissedCommands =
      for {
        missedBodies <- OptionT(getMissedBodiesFun(bestKnownHeaderId).map(NonEmptyChain.fromSeq))
        _ <- OptionT.liftF(Logger[F].info(show"Send request to get missed bodies for blockIds: $missedBodies"))
        message = RequestsProxy.Message.DownloadBlocksRequest(hostId, missedBodies)
        _ <- OptionT.liftF(state.requestsProxy.sendNoWait(message))
      } yield ()

    requestMissedCommands.getOrElse(().pure[F])
  }

  private def processRemoteBodies[F[_]: Async: Logger](
    state:       State[F],
    blockBodies: NonEmptyChain[(BlockId, BlockBody)]
  ): F[(State[F], Response[F])] = {
    val newBodiesOpt = state.bodyStore.filterKnownBlocks(blockBodies)

    val processResult: EitherT[F, String, (State[F], BlockId)] =
      for {
        newBlockBodies      <- EitherT.fromOptionF(newBodiesOpt, "Skip validation of known bodies")
        verifiedFullBlocks  <- verifyAndSaveBodies(state, newBlockBodies)
        appliedNewHeadBlock <- tryToApplyBestBlock(state, verifiedFullBlocks.last.header.id)
        _                   <- EitherT.liftF(requestNextBodyBlocks(state))
        newState = updateState(state, appliedNewHeadBlock)
      } yield (newState, appliedNewHeadBlock)

    processResult
      .biSemiflatTap(
        error => Logger[F].info(show"Failed to apply bodies due: $error"),
        newStateAndBlock => Logger[F].info(show"Successfully adopted block: ${newStateAndBlock._2}")
      )

    // extract current state from Either
    processResult.map(_._1).value.map(_.getOrElse(state)).map(s => (s, s))
  }

  private def verifyAndSaveBodies[F[_]: Async: Logger](
    state:         State[F],
    idToBlockBody: NonEmptyChain[(BlockId, BlockBody)]
  ): EitherT[F, String, NonEmptyChain[Block]] = {
    def verifyAndSaveBody(blockId: BlockId, block: Block) =
      for {
        _ <- Logger[F].info(show"Start verification and applying block $blockId")
        (verifiedId, verifiedBlock) <- verifyBlockBody(state, blockId, block)
          .leftSemiflatTap(e => Logger[F].warn(show"Received invalid block body id=$blockId errors=$e"))
          .leftMap((e: String) => new IllegalArgumentException(e))
          .rethrowT
        _ <- Logger[F].info(show"Saving body id=$verifiedId")
        _ <- state.bodyStore.put(verifiedId, verifiedBlock.body)
      } yield block

    EitherT.fromOptionF(
      idToBlockBody
        .traverse { case (id, body) =>
          Logger[F].info(show"For block: $id try to get header for body") >>
          state.headerStore.getOrRaise(id).map(header => (id, Block(header, body)))
        }
        .flatMap(blocksToApply =>
          Stream
            .foldable(blocksToApply)
            .evalMap { case (blockId, block) => verifyAndSaveBody(blockId, block) }
            .compile
            .toList
            .handleError(_ => List.empty[Block]) // @TODO return actual list of applied bodies
            .map(b => NonEmptyChain.fromSeq(b))
        ),
      "No verified blocks bodies had been found or some block bodies are not correct"
    )
  }

  private def verifyBlockBody[F[_]: Async: Logger](
    state:   State[F],
    blockId: BlockId,
    block:   Block
  ): EitherT[F, String, (BlockId, Block)] = {
    val header = block.header
    val body = block.body
    for {
      _ <- EitherT.liftF(Logger[F].debug(show"Validating header to body consistency for id=$blockId"))
      _ <- EitherT(state.headerToBodyValidation.validate(block)).leftMap(e => e.show)
      _ <- EitherT.liftF(Logger[F].debug(show"Validating syntax of body id=$blockId"))
      _ <- EitherT(state.bodySyntaxValidation.validate(body).map(_.toEither.leftMap(_.show)))
      _ <- EitherT.liftF(Logger[F].debug(show"Validating semantics of body id=$blockId"))
      validationContext = StaticBodyValidationContext(header.parentHeaderId, header.height, header.slot)
      _ <- EitherT(state.bodySemanticValidation.validate(validationContext)(body).map(_.toEither.leftMap(_.show)))
      _ <- EitherT.liftF(Logger[F].debug(show"Validating authorization of body id=$blockId"))
      authValidation = state.bodyAuthorizationValidation.validate(QuivrContext.forConstructedBlock(header, _))(body)
      _ <- EitherT(authValidation.map(_.toEither.leftMap(_.show)))
    } yield (blockId, block)
  }

  private def tryToApplyBestBlock[F[_]: Async](
    state:       BlockChecker.State[F],
    newTopBlock: BlockId
  ): EitherT[F, String, BlockId] = {
    val localChain = state.localChain
    // After fetching and validating all of the data, re-run the chain preference process
    for {
      lastBlockSlotData <- EitherT.fromOptionF(state.slotDataStore.get(newTopBlock), "Failed to get slot data")
      appliedBlock <-
        EitherTExt.condF(
          test = localChain.isWorseThan(lastBlockSlotData),
          ifTrue = localChain.adopt(Validated.Valid(lastBlockSlotData)) >> lastBlockSlotData.slotId.blockId.pure[F],
          ifFalse = show"Ignoring weaker (or equal) block header id=$newTopBlock".pure[F]
        )
    } yield appliedBlock
  }

  private def requestNextBodyBlocks[F[_]: Async](state: State[F]): F[Unit] = {
    val sendMessageCommand =
      for {
        bestTip      <- OptionT.fromOption[F](state.bestKnownRemoteSlotDataOpt.map(_.last))
        missedBodies <- getFirstNMissedInStore(state.bodyStore, state.slotDataStore, bestTip, chunkSize)
        host = state.bestKnownRemoteSlotDataHost.get
      } yield state.requestsProxy.sendNoWait(RequestsProxy.Message.DownloadBlocksRequest(host, missedBodies))

    sendMessageCommand.getOrElse(().pure[F]).flatten
  }

  // clear bestKnownRemoteSlotData at the end of sync, so new slot data will be compared with local chain again
  private def updateState[F[_]](state: State[F], newTopBlock: BlockId): State[F] =
    if (state.bestKnownRemoteSlotDataOpt.exists(_.isLastId(newTopBlock))) {
      state.copy(bestKnownRemoteSlotDataOpt = None)
    } else {
      state
    }
}
