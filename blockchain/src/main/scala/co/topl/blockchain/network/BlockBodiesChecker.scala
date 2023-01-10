package co.topl.blockchain.network

import cats.data._
import cats.effect.Resource
import cats.effect.kernel.Concurrent
import cats.implicits.catsSyntaxApplicativeId
import cats.{Applicative, Order}
import co.topl.blockchain.network.PeersManager.PeersManagerActor
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor
import co.topl.models.{Bytes, TypedIdentifier}

object BlockBodiesChecker {
  sealed trait Message

  object Message {
    case class SetPeerManager[F[_]](peerManagerActor: PeersManagerActor[F]) extends Message
    case class BestHeaders(validHeadersProposal: HeadersCandidate) extends Message
    case class DownloadedBlocks(data: NonEmptyChain[ErrorOrBlock]) extends Message
  }

  case class State[F[_]](
    peersManager:         Option[PeersManagerActor[F]],
    reputationAggregator: ReputationAggregatorActor[F],
    // last element contains more preferable chain because
    // last received headers is better than previous because of how works blocks header checker
    // TODO use SortedSet to not relay on BlocksHeadersChecker assumption
    // store as list to not lost possible valid candidates, could be used if we can't get valid blocks
    // as long candidate will be accepted -- delete all previous candidates
    headerChainCandidates: Seq[HeadersCandidate],

    // cache for downloaded/working blocks.
    // TODO shall be cleaned by some way. All blocks from pre-previous epoch for example?
    // None -- block is requested to download TODO change to case class?
    quickAccessBlocks: Map[TypedIdentifier, ErrorOrBlock]
  )

  type Response[F[_]] = State[F]
  type BlockBodiesCheckerActor[F[_]] = Actor[F, Message, Response[F]]

  implicit val orderTypedIdentifier: Order[TypedIdentifier] =
    Order.by[TypedIdentifier, Bytes](_.allBytes)(Order.from[Bytes]((a, b) => a.compare(b)))

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.BestHeaders(validHeadersProposal)) => acceptNewHeadersProposal(state, validHeadersProposal)
    case (state, Message.DownloadedBlocks(blocks))          => downloadedBlocks(state, blocks)
    case (state, Message.SetPeerManager(peerManager))       => setPeerManager(state, peerManager)
  }

  def makeActor[F[_]: Concurrent](
    reputationAggregatorActor: ReputationAggregatorActor[F]
  ): Resource[F, BlockBodiesCheckerActor[F]] = {
    val initialState = State[F](
      None,
      reputationAggregatorActor,
      Seq.empty[HeadersCandidate],
      Map.empty[TypedIdentifier, ErrorOrBlock]
    )
    Actor.make(initialState, getFsm[F])
  }

  private def acceptNewHeadersProposal[F[_]: Applicative](state: State[F], proposal: HeadersCandidate) = {
    val newCandidates = state.headerChainCandidates :+ proposal

    val quickAccessBlocks = state.quickAccessBlocks

    val blockToDownload =
      NonEmptyChain.fromSeq((proposal.headers.toNes.toSortedSet -- quickAccessBlocks.keys.toSet).toList)

    val newQuickAccessBlocks = blockToDownload
      .map { blocks =>
        val newQuickAccessBlocks: Map[TypedIdentifier, Either[BlockGettingError, NotVerifiedBlock]] =
          quickAccessBlocks ++ blocks.map(id => id -> Left("New Block"))
        state.peersManager.foreach(_.sendNoWait(PeersManager.Message.BlockDownloadRequest(proposal.source, blocks)))
        newQuickAccessBlocks
      }
      .getOrElse(state.quickAccessBlocks)

    val newState = state.copy(headerChainCandidates = newCandidates, quickAccessBlocks = newQuickAccessBlocks)
    (newState, newState).pure[F]
  }

  private def downloadedBlocks[F[_]: Applicative](
    state:     State[F],
    newBlocks: NonEmptyChain[ErrorOrBlock]
  ) = {
    val downloadedBlocks = newBlocks.collect { case Right(block) => block }
    // processing failed to download blocks? ban candidate chain?
    val failedBlocks = newBlocks.collect { case Left(error) => error }

    val quickAccessBlocks: Map[TypedIdentifier, ErrorOrBlock] =
      state.quickAccessBlocks ++
      downloadedBlocks.map(pb => pb.id -> Right(pb)).toList ++
      failedBlocks.map(pb => pb.id -> Left(pb))

    val fullyDownloadedChains =
      state.headerChainCandidates.filter(candidate => candidate.headers.forall(quickAccessBlocks.contains))

    val newCandidates = processCandidates(state, fullyDownloadedChains)

    val newState = state.copy(quickAccessBlocks = quickAccessBlocks, headerChainCandidates = newCandidates)
    (newState, newState).pure[F]
  }

  private def processCandidates[F[_]](
    state:                 State[F],
    fullyDownloadedChains: Seq[HeadersCandidate]
  ): Seq[HeadersCandidate] =
    // select best candidate chain
    // check blocks correctness
    // remove from state.headerChainCandidates chains which are worse successfully applied chain
    state.headerChainCandidates

  private def setPeerManager[F[_]: Applicative](state: State[F], peersManagerActor: PeersManagerActor[F]) = {
    val newState = state.copy(peersManager = Option(peersManagerActor))
    (newState, newState).pure[F]
  }
}
