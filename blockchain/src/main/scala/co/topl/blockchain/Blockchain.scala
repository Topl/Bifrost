package co.topl.blockchain

import cats.data._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain.interpreters.RegtestRpcServer
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.catsutils._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig.Bifrost.{KnownPeer, NetworkProperties}
import co.topl.grpc._
import co.topl.ledger.implicits._
import co.topl.ledger.interpreters.{QuivrContext, TransactionSemanticValidation}
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters._
import co.topl.models.p2p._
import co.topl.networking.blockchain._
import co.topl.networking.fsnetwork.DnsResolverInstances.DefaultDnsResolver
import co.topl.networking.fsnetwork.P2PShowInstances._
import co.topl.networking.fsnetwork.ReverseDnsResolverInstances.{DefaultReverseDnsResolver, NoOpReverseResolver}
import co.topl.networking.fsnetwork._
import co.topl.networking.p2p._
import co.topl.node.models.{Block, BlockBody, FullBlock, KnownHost}
import co.topl.typeclasses.implicits._
import com.comcast.ip4s.Dns
import fs2.concurrent.Topic
import fs2.{io => _, _}
import io.grpc.ServerServiceDefinition
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._

object Blockchain {

  /**
   * A program which executes the blockchain protocol, including a P2P layer, RPC layer, and minter.
   */
  def make[F[_]: Async: Random: Dns: Stats](
    localBlockchain:        BlockchainCore[F],
    p2pBlockchain:          BlockchainCore[F],
    stakerResource:         Resource[F, Option[StakingAlgebra[F]]],
    eventSourcedStates:     EventSourcedStates[F],
    localPeer:              LocalPeer,
    knownPeers:             List[KnownPeer],
    rpcHost:                String,
    rpcPort:                Int,
    additionalGrpcServices: List[ServerServiceDefinition],
    peerAsServer:           Option[KnownPeer],
    networkProperties:      NetworkProperties,
    regtestEnabled:         Boolean
  ): Resource[F, Unit] = new BlockchainImpl[F](
    localBlockchain,
    p2pBlockchain,
    stakerResource,
    eventSourcedStates,
    localPeer,
    knownPeers,
    rpcHost,
    rpcPort,
    additionalGrpcServices,
    peerAsServer,
    networkProperties,
    regtestEnabled
  ).resource

}

class BlockchainImpl[F[_]: Async: Random: Dns: Stats](
  localBlockchain:        BlockchainCore[F],
  p2pBlockchain:          BlockchainCore[F],
  stakerResource:         Resource[F, Option[StakingAlgebra[F]]],
  eventSourcedStates:     EventSourcedStates[F],
  localPeer:              LocalPeer,
  knownPeers:             List[KnownPeer],
  rpcHost:                String,
  rpcPort:                Int,
  additionalGrpcServices: List[ServerServiceDefinition],
  peerAsServer:           Option[KnownPeer],
  networkProperties:      NetworkProperties,
  regtestEnabled:         Boolean
) {
  implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")

  /**
   * For each adopted block, trigger all internal event-sourced states to update.  Generally, EventSourcedStates are
   * lazily evaluated.  In some cases, they may not be evaluated for days at a time depending on user-behavior.  Once
   * finally triggered, this causes a major CPU burden for a period of time while the state updates.  To avoid this,
   * we eagerly evaluate each state based on the canonical head.
   */
  private def eventSourcedStateUpdater =
    Resource.make(Logger[F].info("Initializing Event-Sourced-State Updater"))(_ =>
      Logger[F].info("Event-Sourced-State Updater Terminated")
    ) >>
    Stream
      .force(localBlockchain.consensus.localChain.adoptions)
      .dropOldest(1)
      .evalTap(eventSourcedStates.updateLocalStatesTo)
      .compile
      .drain
      .onError { case e => Logger[F].error(e)("Event-Sourced-State Updater failed") }
      .background
      .void

  private def p2p =
    for {
      _           <- Resource.make(Logger[F].info("Initializing P2P"))(_ => Logger[F].info("P2P Terminated"))
      remotePeers <- Queue.unbounded[F, DisconnectedPeer].toResource
      peersStatusChangesTopic <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)
      _                       <- Logger[F].info(s"Received known peers from config: $knownPeers").toResource
      currentPeers            <- Ref.of[F, Set[RemotePeer]](Set.empty[RemotePeer]).toResource
      initialPeers = knownPeers.map(kp => DisconnectedPeer(RemoteAddress(kp.host, kp.port), none))
      remotePeersStream = Stream.fromQueueUnterminated[F, DisconnectedPeer](remotePeers)
      implicit0(dnsResolver: DnsResolver[F]) = new DefaultDnsResolver[F]()
      implicit0(reverseDnsResolver: ReverseDnsResolver[F]) =
        if (networkProperties.useHostNames) new DefaultReverseDnsResolver[F]() else new NoOpReverseResolver[F]
      bridge <- ActorPeerHandlerBridgeAlgebra
        .make(
          HostId(localPeer.p2pVK),
          p2pBlockchain,
          networkProperties,
          initialPeers,
          peersStatusChangesTopic,
          remotePeers.offer,
          currentPeers.set,
          p2pBlockchain.cryptoResources.ed25519VRF
        )
        .onFinalize(Logger[F].info("P2P Actor system had been shutdown"))
      _ <- Logger[F].info(s"Exposing server on: ${peerAsServer.fold("")(_.toString)}").toResource
      peerServerF = BlockchainPeerServer.make(
        p2pBlockchain,
        () => peerAsServer.map(kp => KnownHost(localPeer.p2pVK, kp.host, kp.port)),
        () => currentPeers.get,
        peersStatusChangesTopic
      ) _
      _ <- BlockchainNetwork
        .make[F](
          localPeer.localAddress.host,
          localPeer.localAddress.port,
          localPeer,
          remotePeersStream,
          bridge,
          peerServerF,
          peersStatusChangesTopic,
          p2pBlockchain.cryptoResources.ed25519,
          networkProperties.defaultTimeout
        )
    } yield ()

  def rpc(regtestPermitQueue: Option[Queue[F, Unit]]): Resource[F, Unit] =
    for {
      _               <- Resource.make(Logger[F].info("Initializing RPC"))(_ => Logger[F].info("RPC Terminated"))
      rpcInterpreter  <- ToplRpcServer.make(localBlockchain).toResource
      nodeGrpcService <- NodeGrpc.Server.service[F](rpcInterpreter)
      regtestServices <- regtestPermitQueue.toList.traverse(queue =>
        RegtestRpcServer.service[F](Async[F].defer(queue.offer(())))
      )
      rpcServer <- ToplGrpc.Server
        .serve(rpcHost, rpcPort)(nodeGrpcService :: regtestServices ++ additionalGrpcServices)
      _ <- Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}").toResource
    } yield ()

  private def blockProduction(regtestPermitQueue: Option[Queue[F, Unit]]): Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing local blocks (potential no-op)"))(_ =>
        Logger[F].info("Local blocks terminated")
      )
      mintedBlockStream =
        for {
          stakerOpt <- Stream.resource(stakerResource)
          staker    <- Stream.fromOption[F](stakerOpt)
          blockPackerValidation <- Stream.resource(
            TransactionSemanticValidation
              .makeDataValidation(localBlockchain.dataStores.transactions.getOrRaise)
              .flatMap(
                BlockPackerValidation.make[F](_, localBlockchain.ledger.transactionAuthorizationValidation)
              )
          )
          blockPacker <- Stream.resource(
            BlockPacker
              .make[F](
                localBlockchain.ledger.mempool,
                localBlockchain.validators.boxState,
                localBlockchain.validators.rewardCalculator,
                localBlockchain.ledger.transactionCostCalculator,
                blockPackerValidation,
                localBlockchain.validators.registrationAccumulator
              )
          )
          // The BlockProducer needs a stream/Source of "parents" upon which it should build.  This stream is the
          // concatenation of the current local head with the stream of local block adoptions
          parentBlocksStream = Stream
            .eval(Sync[F].defer(localBlockchain.consensus.localChain.head))
            .evalTap(head => localBlockchain.clock.delayedUntilSlot(head.slotId.slot))
            .append(
              Stream
                .force(localBlockchain.consensus.localChain.adoptions)
                .dropOldest(1)
                .evalMap(localBlockchain.dataStores.slotData.getOrRaise)
            )
          productionPermit =
            regtestPermitQueue.fold(().pure[F])(regtestPermitQueue =>
              Async[F].defer(Logger[F].info("Awaiting RegtestRpc.MakeBlock") >> regtestPermitQueue.take)
            )
          blockProducer <- Stream.eval(
            BlockProducer.make[F](
              parentBlocksStream,
              staker,
              localBlockchain.clock,
              blockPacker,
              localBlockchain.validators.rewardCalculator,
              productionPermit
            )
          )
          block <- Stream.force(blockProducer.blocks)
        } yield block
      _ <- mintedBlockStream
        .evalTap(block => Logger[F].info(show"Saving locally-produced block id=${block.header.id}"))
        .evalTap { block =>
          val id = block.header.id
          localBlockchain.blockIdTree.associate(id, block.header.parentHeaderId) &>
          localBlockchain.dataStores.headers.put(id, block.header) &>
          localBlockchain.dataStores.bodies
            .put(id, BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))) &>
          block.fullBody.rewardTransaction.traverse(tx => localBlockchain.dataStores.transactions.put(tx.id, tx)) &>
          localBlockchain.cryptoResources.ed25519VRF
            .use(implicit e => Sync[F].delay(block.header.slotData))
            .flatTap(localBlockchain.dataStores.slotData.put(block.header.id, _))
        }
        // Validate the local block.  If invalid, skip it "gracefully"
        .evalFilter(validateLocalBlock(_).toOption.isDefined)
        .evalTap(block =>
          localBlockchain.dataStores.slotData
            .getOrRaise(block.header.id)
            .flatMap(slotData =>
              localBlockchain.consensus.localChain
                .isWorseThan(NonEmptyChain.one(slotData))
                .ifM(
                  localBlockchain.consensus.localChain.adopt(Validated.Valid(slotData)),
                  Logger[F].warn("Skipping adoption of locally-produced block due to better local chain.")
                )
            )
        )
        .compile
        .drain
        .onError { case e => Logger[F].error(e)("Block producer failed") }
        .background
    } yield ()

  def resource: Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing Blockchain"))(_ => Logger[F].info("Blockchain Terminated"))
      // When regtest mode is enabled, allocate a queue to hold commands to produce new blocks
      regtestPermitQueue <-
        if (regtestEnabled) Queue.unbounded[F, Unit].toResource.map(_.some)
        else none[Queue[F, Unit]].pure[F].toResource
      _ <- (
        p2p,
        rpc(regtestPermitQueue),
        blockProduction(regtestPermitQueue),
        eventSourcedStateUpdater
      ).parTupled
      _ <- Resource.never[F, Unit]
    } yield ()

  /**
   * Performs all header+body validations of the given block.  If invalid, deletes the header and body from storage and
   * logs a warning.
   */
  private def validateLocalBlock(fullBlock: FullBlock) =
    (for {
      _ <- EitherT.liftF[F, String, Unit](
        Logger[F].info(show"Performing validation of local blockId=${fullBlock.header.id}")
      )
      _ <- EitherT(
        localBlockchain.validators.header
          .validate(fullBlock.header)
          .warnIfSlow("Validate local header")
      ).leftMap(_.show)
      body <- EitherT.liftF(
        Sync[F]
          .delay(
            BlockBody(fullBlock.fullBody.transactions.map(_.id), fullBlock.fullBody.rewardTransaction.map(_.id))
          )
      )
      block = Block(fullBlock.header, body)
      _ <- EitherT(
        localBlockchain.validators.headerToBody
          .validate(block)
          .warnIfSlow("Validate local header-to-body")
      ).leftMap(_.show)
      _ <- EitherT(
        localBlockchain.validators.bodySyntax
          .validate(body)
          .map(_.toEither)
          .warnIfSlow("Validate local body syntax")
      ).leftMap(_.show)
      semanticContext = StaticBodyValidationContext(
        block.header.parentHeaderId,
        block.header.height,
        block.header.slot
      )
      _ <- EitherT(
        localBlockchain.validators.bodySemantics
          .validate(semanticContext)(body)
          .map(_.toEither)
          .warnIfSlow("Validate local body semantics")
      ).leftMap(_.show)
      authContext = (tx: IoTransaction) => QuivrContext.forProposedBlock(block.header.height, block.header.slot, tx)
      _ <- EitherT(
        localBlockchain.validators.bodyAuthorization
          .validate(authContext)(body)
          .map(_.toEither)
          .warnIfSlow("Validate local body authorization")
      ).leftMap(_.show)
      _ <- EitherT.liftF[F, String, Unit](Logger[F].info(show"Local blockId=${fullBlock.header.id} is valid"))
    } yield ())
      .leftSemiflatTap(reason =>
        Logger[F].warn(show"Locally produced block id=${fullBlock.header.id} is invalid. reason=$reason") &>
        localBlockchain.dataStores.headers.remove(fullBlock.header.id) &>
        localBlockchain.dataStores.bodies.remove(fullBlock.header.id) &>
        fullBlock.fullBody.rewardTransaction.traverseTap(tx => localBlockchain.dataStores.transactions.remove(tx.id))
      )
}
