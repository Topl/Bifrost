package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{EitherT, NonEmptyChain, OptionT, Validated}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.ledger.algebras._
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.models.{Block, TypedIdentifier}
import co.topl.networking.fsnetwork.BlockChecker.Message._
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import co.topl.models.utility._

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
     * @param slotData slot data to compare
     */
    case class RemoteSlotData(source: HostId, slotData: NonEmptyChain[SlotData]) extends Message

    /**
     * Check and adopt remote headers, if headers is valid then appropriate bodies will be requested
     * @param source source of headers, used as a hint from which peer bodies shall be requested
     * @param headers headers to check and adopt
     */
    case class RemoteBlockHeader(source: HostId, headers: NonEmptyChain[(TypedIdentifier, BlockHeader)]) extends Message

    /**
     * check and adopt block bodies, if adopted bodies is better than local chain then remote bodies became new top block
     * @param source source of bodies, could be used as a hint from which peer next bodies shall be requested
     * @param bodies bodies to check
     */
    case class RemoteBlockBodies(source: HostId, bodies: NonEmptyChain[(TypedIdentifier, BlockBody)]) extends Message
    // TODO implement it, in case if verification of some incoming data fot current best slot is failed
    //  then we need to reset current best slot data (and request it again) to not stuck on incorrect slot data
    // case class ClearSlotData() extends Message
  }

  case class State[F[_]](
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
    chainSelection:              ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    // TODO maybe use some kind of stack to not loose previous best slot data
    bestKnownRemoteSlotData: Option[SlotData],
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
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData]
  ): Resource[F, BlockCheckerActor[F]] = {
    val initialState =
      State(
        reputationAggregator,
        peersManager,
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
        None,
        None
      )
    Actor.make(initialState, getFsm[F])
  }

  private def processSlotData[F[_]: Async: Logger](
    state:           State[F],
    candidateHostId: HostId,
    slotData:        NonEmptyChain[SlotData]
  ): F[(State[F], Response[F])] = {
    val remoteSlotData = slotData.last
    for {
      _                 <- Logger[F].info(show"Received slot data proposal ${remoteSlotData.slotId.blockId}")
      localBestSlotData <- state.bestKnownRemoteSlotData.map(_.pure[F]).getOrElse(state.localChain.head)
      newState <- state.chainSelection
        .firstIsBetter(remoteSlotData, localBestSlotData)
        .ifM(
          ifTrue = changeLocalSlotData(state, remoteSlotData, candidateHostId).flatTap(s => requestNextHeaders(s)),
          ifFalse = Logger[F].debug(s"Ignore weaker slot data ${remoteSlotData.slotId.blockId}") >> state.pure[F]
        )
    } yield (newState, newState)
  }

  private def changeLocalSlotData[F[_]: Async: Logger](
    state:       State[F],
    newSlotData: SlotData,
    hostId:      HostId
  ): F[State[F]] =
    Logger[F].info(show"Update best local slot data with ${newSlotData.slotId.blockId}") >>
    state.copy(bestKnownRemoteSlotData = Option(newSlotData), bestKnownRemoteSlotDataHost = Option(hostId)).pure[F]

  private def requestNextHeaders[F[_]: MonadThrow: Logger](state: State[F]): F[Unit] =
    state.bestKnownRemoteSlotData
      .map { bestSlot =>
        getFirstNMissedInStore(state.headerStore, state.slotDataStore, bestSlot, chunkSize)
          .flatTap(m => OptionT.liftF(Logger[F].info(show"Send request to get missed headers for blockIds: $m")))
          .map(PeersManager.Message.BlockHeadersRequest(state.bestKnownRemoteSlotDataHost.get, _))
          .foreachF(state.peersManager.sendNoWait)
      }
      .getOrElse(().pure[F])

  private def processRemoteHeaders[F[_]: Async: Logger](
    state:        State[F],
    hostId:       HostId,
    blockHeaders: NonEmptyChain[(TypedIdentifier, BlockHeader)]
  ): F[(State[F], Response[F])] = {
    val processResult =
      for {
        _ <- EitherT.right(Logger[F].info(show"Received headers: ${blockHeaders.map(_._1)}"))
        newHeadersOpt = state.headerStore.filterKnownBlocks(blockHeaders)
        newHeaders      <- EitherT.fromOptionF(newHeadersOpt, "Skip validation of known headers")
        verifiedHeaders <- verifyAndSaveHeaders(state, newHeaders)
        // TODO finish processing here if incoming headers is not part of current best chain
        _ <- EitherT.right[String](requestBlockBodies(state, hostId, verifiedHeaders))
        _ <- EitherT.right[String](requestNextHeaders(state))
      } yield verifiedHeaders

    processResult
      .biSemiflatTap(
        error => Logger[F].info(show"Failed to apply headers due: $error"),
        newHeaders => Logger[F].info(show"Successfully adopted headers: ${newHeaders.map(_._1)}")
      )
      .value as (state, state)
  }

  private def verifyAndSaveHeaders[F[_]: Async: Logger](
    state:        State[F],
    blockHeaders: NonEmptyChain[(TypedIdentifier, BlockHeader)]
  ): EitherT[F, String, NonEmptyChain[(TypedIdentifier, BlockHeader)]] = {
    def verifyAndSaveHeader(headerId: TypedIdentifier, header: BlockHeader) =
      for {
        _ <- Logger[F].debug(show"Validating remote header id=$headerId")
        validationFunction = state.headerValidation.validate(header, _)
        _ <- EitherT(state.headerStore.getOrRaise(header.parentHeaderId: TypedIdentifier).flatMap(validationFunction))
          .leftSemiflatTap(error => Logger[F].warn(show"Received invalid block header id=$headerId error=$error"))
          .leftMap(error => new IllegalArgumentException(error.show))
          .rethrowT
        _ <- Logger[F].info(show"Saving header id=$headerId")
        _ <- state.headerStore.put(headerId, header)
      } yield (headerId, header)

    EitherT.fromOptionF(
      Stream
        .foldable(blockHeaders)
        .evalMap { case (headerId, header) => verifyAndSaveHeader(headerId, header) }
        .compile
        .toList
        .handleError(_ => List.empty[(TypedIdentifier, BlockHeader)]) // @TODO return actual list of applied bodies
        .map(b => NonEmptyChain.fromSeq(b)),
      "No verified blocks headers had been found or some block headers are not correct"
    )
  }

  private def requestBlockBodies[F[_]: Async: Logger](
    state:           State[F],
    hostId:          HostId,
    verifiedHeaders: NonEmptyChain[(TypedIdentifier, BlockHeader)]
  ): F[Unit] = {
    val blockIds: NonEmptyChain[TypedIdentifier] = verifiedHeaders.map(_._1)
    Logger[F].info(show"Send request to get missed bodies for blockIds: $blockIds") >>
    state.peersManager.sendNoWait(PeersManager.Message.BlockDownloadRequest(hostId, blockIds))
  }

  private def processRemoteBodies[F[_]: Async: Logger](
    state:       State[F],
    blockBodies: NonEmptyChain[(TypedIdentifier, BlockBody)]
  ): F[(State[F], Response[F])] = {
    val newBodiesOpt = state.bodyStore.filterKnownBlocks(blockBodies)
    val processResult =
      for {
        newBlockBodies      <- EitherT.fromOptionF(newBodiesOpt, "Skip validation of known bodies")
        verifiedFullBlocks  <- verifyAndSaveBodies(state, newBlockBodies)
        appliedNewHeadBlock <- tryToApplyBestBlock(state, verifiedFullBlocks.last.header.id.asTypedBytes)
      } yield appliedNewHeadBlock

    processResult
      .biSemiflatTap(
        error => Logger[F].info(show"Failed to apply bodies due: $error"),
        newBestBlock => Logger[F].info(show"Successfully adopted block: $newBestBlock")
      )
      .value as (state, state)
  }

  private def verifyAndSaveBodies[F[_]: Async: Logger](
    state:         State[F],
    idToBlockBody: NonEmptyChain[(TypedIdentifier, BlockBody)]
  ): EitherT[F, String, NonEmptyChain[Block]] = {
    def verifyAndSaveBody(blockId: TypedIdentifier, block: Block) =
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
    blockId: TypedIdentifier,
    block:   Block
  ): EitherT[F, String, (TypedIdentifier, Block)] = {
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
      authValidation = state.bodyAuthorizationValidation.validate(header.parentHeaderId)(body)
      _ <- EitherT(authValidation.map(_.toEither.leftMap(_.show)))
    } yield (blockId, block)
  }

  private def tryToApplyBestBlock[F[_]: Async](
    state:       BlockChecker.State[F],
    newTopBlock: TypedIdentifier
  ): EitherT[F, String, BlockId] =
    // After fetching and validating all of the data, re-run the chain preference process
    for {
      lastBlockSlotData <- EitherT.fromOptionF(state.slotDataStore.get(newTopBlock), "Failed to get slot data")
      appliedBlock <-
        EitherTExt.condF(
          state.localChain.isWorseThan(lastBlockSlotData),
          state.localChain.adopt(Validated.Valid(lastBlockSlotData)) >> lastBlockSlotData.slotId.blockId.pure[F],
          show"Ignoring weaker (or equal) block header id=$newTopBlock".pure[F]
        )
    } yield appliedBlock
}
