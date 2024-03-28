package co.topl.blockchain

import cats.data._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation._
import co.topl.catsutils._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig.Bifrost.{KnownPeer, NetworkProperties}
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.eventtree.ParentChildTree
import co.topl.grpc._
import co.topl.ledger.algebras._
import co.topl.ledger.implicits._
import co.topl.ledger.interpreters.{QuivrContext, TransactionSemanticValidation}
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters._
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
  def make[F[_]: Async: Random: Dns](
    clock:                     ClockAlgebra[F],
    stakerResource:            Resource[F, Option[StakingAlgebra[F]]],
    dataStores:                DataStores[F],
    localChain:                LocalChainAlgebra[F],
    chainSelectionAlgebra:     ChainSelectionAlgebra[F, SlotData],
    blockIdTree:               ParentChildTree[F, BlockId],
    eventSourcedStates:        EventSourcedStates[F],
    validatorsLocal:           Validators[F],
    validatorsP2P:             Validators[F],
    _mempool:                  MempoolAlgebra[F],
    cryptoResources:           CryptoResources[F],
    localPeer:                 LocalPeer,
    knownPeers:                List[KnownPeer],
    rpcHost:                   String,
    rpcPort:                   Int,
    nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
    additionalGrpcServices:    List[ServerServiceDefinition],
    _epochData:                EpochDataAlgebra[F],
    peerAsServer:              Option[KnownPeer],
    networkProperties:         NetworkProperties
  ): Resource[F, Unit] = new BlockchainImpl[F](
    clock,
    stakerResource,
    dataStores,
    localChain,
    chainSelectionAlgebra,
    blockIdTree,
    eventSourcedStates,
    validatorsLocal,
    validatorsP2P,
    _mempool,
    cryptoResources,
    localPeer,
    knownPeers,
    rpcHost,
    rpcPort,
    nodeProtocolConfiguration,
    additionalGrpcServices,
    _epochData,
    peerAsServer,
    networkProperties
  ).resource

}

class BlockchainImpl[F[_]: Async: Random: Dns](
  clock:                     ClockAlgebra[F],
  stakerResource:            Resource[F, Option[StakingAlgebra[F]]],
  dataStores:                DataStores[F],
  localChain:                LocalChainAlgebra[F],
  chainSelectionAlgebra:     ChainSelectionAlgebra[F, SlotData],
  blockIdTree:               ParentChildTree[F, BlockId],
  eventSourcedStates:        EventSourcedStates[F],
  validatorsLocal:           Validators[F],
  validatorsP2P:             Validators[F],
  _mempool:                  MempoolAlgebra[F],
  cryptoResources:           CryptoResources[F],
  localPeer:                 LocalPeer,
  knownPeers:                List[KnownPeer],
  rpcHost:                   String,
  rpcPort:                   Int,
  nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
  additionalGrpcServices:    List[ServerServiceDefinition],
  _epochData:                EpochDataAlgebra[F],
  peerAsServer:              Option[KnownPeer],
  networkProperties:         NetworkProperties
) {
  implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")

  /**
   * Whenever a block is adopted locally, broadcast all of its corresponding (non-reward) _transactions_ to eagerly notify peers
   */
  private def adoptedBlockTxRebroadcaster(transactionsTopic: Topic[F, TransactionId]) =
    Resource.make(Logger[F].info("Initializing Block-Tx Rebroadcaster"))(_ =>
      Logger[F].info("Block-Tx Rebroadcaster Terminated")
    ) >>
    Stream
      .force(localChain.adoptions)
      .dropOldest(10)
      .evalMap(id => dataStores.bodies.getOrRaise(id))
      .flatMap(b => Stream.iterable(b.transactionIds))
      .through(transactionsTopic.publish)
      .compile
      .drain
      .onError { case e => Logger[F].error(e)("Block-Tx Rebroadcaster failed") }
      .background

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
      .force(localChain.adoptions)
      .dropOldest(1)
      .evalTap(eventSourcedStates.updateLocalStatesTo)
      .compile
      .drain
      .onError { case e => Logger[F].error(e)("Event-Sourced-State Updater failed") }
      .background
      .void

  private def p2p(mempool: MempoolAlgebra[F], transactionsTopic: Topic[F, TransactionId]) =
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
          localChain,
          chainSelectionAlgebra,
          validatorsP2P.header,
          validatorsP2P.headerToBody,
          validatorsP2P.transactionSyntax,
          validatorsP2P.bodySyntax,
          validatorsP2P.bodySemantics,
          validatorsP2P.bodyAuthorization,
          dataStores.slotData,
          dataStores.headers,
          dataStores.bodies,
          dataStores.transactions,
          dataStores.knownHosts,
          blockIdTree,
          eventSourcedStates.blockHeightsP2P,
          mempool,
          networkProperties,
          clock,
          initialPeers,
          peersStatusChangesTopic,
          remotePeers.offer,
          currentPeers.set
        )
        .onFinalize(Logger[F].info("P2P Actor system had been shutdown"))
      p2pBlockAdoptionsTopic <- Resource.make(Topic[F, BlockId])(_.close.void)
      _ <- Stream
        .force(localChain.adoptions)
        .dropOldest(1)
        .through(p2pBlockAdoptionsTopic.publish)
        .compile
        .drain
        .background
      _ <- Logger[F].info(s"Exposing server on: ${peerAsServer.map(_.toString).getOrElse("")}").toResource
      peerServerF = BlockchainPeerServer.make(
        dataStores.slotData.get,
        dataStores.headers.get,
        dataStores.bodies.get,
        dataStores.transactions.get,
        eventSourcedStates.blockHeightsP2P,
        () => peerAsServer.map(kp => KnownHost(localPeer.p2pVK, kp.host, kp.port)),
        () => currentPeers.get,
        localChain,
        mempool,
        p2pBlockAdoptionsTopic,
        transactionsTopic,
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
          cryptoResources.ed25519
        )
    } yield ()

  def rpc(mempool: MempoolAlgebra[F]): Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing RPC"))(_ => Logger[F].info("RPC Terminated"))
      rpcInterpreter <- ToplRpcServer
        .make(
          dataStores.headers,
          dataStores.bodies,
          dataStores.transactions,
          mempool,
          validatorsLocal.transactionSyntax,
          localChain,
          eventSourcedStates.blockHeightsLocal,
          blockIdTree,
          Stream.force(localChain.adoptions).dropOldest(10),
          nodeProtocolConfiguration,
          _epochData,
          clock
        )
        .toResource
      nodeGrpcService <- NodeGrpc.Server.service[F](rpcInterpreter)
      rpcServer <- ToplGrpc.Server
        .serve(rpcHost, rpcPort)(nodeGrpcService :: additionalGrpcServices)
      _ <- Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}").toResource
    } yield ()

  def blockProduction(mempool: MempoolAlgebra[F]): Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing local blocks (potential no-op)"))(_ =>
        Logger[F].info("Local blocks terminated")
      )
      mintedBlockStream =
        for {
          stakerOpt <- Stream.resource(stakerResource)
          staker    <- Stream.fromOption[F](stakerOpt)
          costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
          blockPackerValidation <- Stream.resource(
            TransactionSemanticValidation
              .makeDataValidation(dataStores.transactions.getOrRaise)
              .flatMap(
                BlockPackerValidation.make[F](_, validatorsLocal.transactionAuthorization)
              )
          )
          blockPacker <- Stream.resource(
            BlockPacker
              .make[F](
                mempool,
                validatorsLocal.boxState,
                validatorsLocal.rewardCalculator,
                costCalculator,
                blockPackerValidation,
                validatorsLocal.registrationAccumulator
              )
          )
          // The BlockProducer needs a stream/Source of "parents" upon which it should build.  This stream is the
          // concatenation of the current local head with the stream of local block adoptions
          parentBlocksStream = Stream
            .eval(Sync[F].defer(localChain.head))
            .evalTap(head => clock.delayedUntilSlot(head.slotId.slot))
            .append(
              Stream
                .force(localChain.adoptions)
                .dropOldest(1)
                .evalMap(dataStores.slotData.getOrRaise)
            )
          blockProducer <- Stream.eval(
            BlockProducer.make[F](parentBlocksStream, staker, clock, blockPacker, validatorsLocal.rewardCalculator)
          )
          block <- Stream.force(blockProducer.blocks)
        } yield block
      _ <- mintedBlockStream
        .evalTap(block => Logger[F].info(show"Saving locally-produced block id=${block.header.id}"))
        .evalTap { block =>
          val id = block.header.id
          blockIdTree.associate(id, block.header.parentHeaderId) &>
          dataStores.headers.put(id, block.header) &>
          dataStores.bodies
            .put(id, BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))) &>
          block.fullBody.rewardTransaction.traverse(tx => dataStores.transactions.put(tx.id, tx)) &>
          cryptoResources.ed25519VRF
            .use(implicit e => Sync[F].delay(block.header.slotData))
            .flatTap(dataStores.slotData.put(block.header.id, _))
        }
        // Validate the local block.  If invalid, skip it "gracefully"
        .evalFilter(validateLocalBlock(_).toOption.isDefined)
        .evalTap(block =>
          dataStores.slotData
            .getOrRaise(block.header.id)
            .flatMap(slotData =>
              localChain
                .isWorseThan(slotData)
                .ifM(
                  localChain.adopt(Validated.Valid(slotData)),
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
      (mempool, transactionsTopic) <- MempoolBroadcaster.make(_mempool)
      _ <- (
        adoptedBlockTxRebroadcaster(transactionsTopic),
        p2p(mempool, transactionsTopic),
        rpc(mempool),
        blockProduction(mempool),
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
        validatorsLocal.header
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
        validatorsLocal.headerToBody
          .validate(block)
          .warnIfSlow("Validate local header-to-body")
      ).leftMap(_.show)
      _ <- EitherT(
        validatorsLocal.bodySyntax
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
        validatorsLocal.bodySemantics
          .validate(semanticContext)(body)
          .map(_.toEither)
          .warnIfSlow("Validate local body semantics")
      ).leftMap(_.show)
      authContext = (tx: IoTransaction) => QuivrContext.forProposedBlock(block.header.height, block.header.slot, tx)
      _ <- EitherT(
        validatorsLocal.bodyAuthorization
          .validate(authContext)(body)
          .map(_.toEither)
          .warnIfSlow("Validate local body authorization")
      ).leftMap(_.show)
      _ <- EitherT.liftF[F, String, Unit](Logger[F].info(show"Local blockId=${fullBlock.header.id} is valid"))
    } yield ())
      .leftSemiflatTap(reason =>
        Logger[F].warn(show"Locally produced block id=${fullBlock.header.id} is invalid. reason=$reason") &>
        dataStores.headers.remove(fullBlock.header.id) &>
        dataStores.bodies.remove(fullBlock.header.id) &>
        fullBlock.fullBody.rewardTransaction.traverseTap(tx => dataStores.transactions.remove(tx.id))
      )
}
