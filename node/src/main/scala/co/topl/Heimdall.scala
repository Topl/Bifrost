package co.topl

import akka.actor.typed._
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.{ActorRef => CActorRef}
import akka.http.scaladsl.Http
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.akkahttprpc.{ThrowableData, ThrowableSupport}
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.consensus._
import co.topl.http.HttpService
import co.topl.network._
import co.topl.network.utils.NetworkTimeProvider
import co.topl.nodeView._
import co.topl.rpc.ToplRpcServer
import co.topl.settings.{AppContext, AppSettings}
import co.topl.tools.exporter.MongoDBOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider
import io.circe.Encoder

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * Heimdall is the guardian of Bifrost.  More specifically, it is the "guardian actor"
 * for the Bifrost application's ActorSystem.
 */
object Heimdall {

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    private[Heimdall] case object NodeViewHolderReady extends ReceivableMessage

    private[Heimdall] case object NetworkControllerReady extends ReceivableMessage

    private[Heimdall] case object BindExternalTraffic extends ReceivableMessage

    private[Heimdall] case class P2PTrafficBound(address: InetSocketAddress) extends ReceivableMessage

    private[Heimdall] case class RPCTrafficBound(httpService: HttpService, binding: Http.ServerBinding)
        extends ReceivableMessage

    case class Fail(throwable: Throwable) extends ReceivableMessage
  }

  /**
   * A guardian behavior which creates all of the child actors needed to run Bifrost.
   */
  def apply(settings: AppSettings, appContext: AppContext): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit def system: ActorSystem[_] = context.system
      implicit def networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

      implicit val timeout: Timeout = Timeout(10.minutes)
      implicit val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)(context.system)
      implicit val protocolVersioner: ProtocolVersioner =
        ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)

      context.log.info("Initializing KeyManager, ConsensusHolder, and NodeViewHolder")

      val state = prepareNodeViewState(settings)

      context.watch(state.keyManager)
      context.watch(state.nodeViewHolder)

      context.pipeToSelf(new ActorNodeViewHolderInterface(state.nodeViewHolder).onReady()) {
        case Failure(exception) => ReceivableMessages.Fail(exception)
        case Success(_)         => ReceivableMessages.NodeViewHolderReady
      }

      awaitingNodeViewReady(settings, appContext, state)(timeProvider, protocolVersioner)
    }

  /**
   * The state in which a KeyManager and NodeViewHolder exist, but the NodeViewHolder is still loading and can't
   * handle any traffic yet.  Heimdall delays creating the rest of the actors until the NodeViewHolder is ready.
   */
  private def awaitingNodeViewReady(
    settings:              AppSettings,
    appContext:            AppContext,
    state:                 NodeViewInitializingState
  )(implicit timeProvider: TimeProvider, protocolVersioner: ProtocolVersioner): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (context, ReceivableMessages.NodeViewHolderReady) =>
        implicit def ctx: ActorContext[ReceivableMessage] = context
        implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

        context.log.info("Initializing PeerManager, NetworkController, Forger, and MemPoolAuditor")
        val nextState = prepareNetworkControllerState(settings, appContext, state)
        context.watch(nextState.forger)
        context.watch(nextState.networkController)
        context.watch(nextState.peerManager)
        context.watch(nextState.mempoolAuditor)
        context.self.tell(ReceivableMessages.NetworkControllerReady)
        awaitingNetworkControllerReady(settings, appContext, nextState)
      case (_, ReceivableMessages.Fail(throwable)) =>
        throw throwable
    }

  private def awaitingNetworkControllerReady(
    settings:              AppSettings,
    appContext:            AppContext,
    state:                 NetworkControllerInitializingState
  )(implicit timeProvider: TimeProvider): Behavior[ReceivableMessage] =
    Behaviors.receivePartial { case (context, ReceivableMessages.NetworkControllerReady) =>
      context.log.info(
        "Initializing PeerSynchronizer, NodeViewSynchronizer, and ChainReplicator"
      )

      implicit def ctx: ActorContext[ReceivableMessage] = context

      val nextState = prepareRemainingActors(settings, state)

      context.watch(nextState.peerSynchronizer)
      context.watch(nextState.nodeViewSynchronizer)
      nextState.chainReplicator match {
        case Some(chainReplicator) => context.watch(chainReplicator)
        case None                  =>
      }

      context.self.tell(ReceivableMessages.BindExternalTraffic)

      awaitingBindExternalTraffic(settings, appContext, nextState)
    }

  /**
   * The Heimdall state in which the child actors have been created but more initialization is needed.
   */
  private def awaitingBindExternalTraffic(
    settings:   AppSettings,
    appContext: AppContext,
    state:      ActorsInitializedState
  ): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (context, ReceivableMessages.BindExternalTraffic) =>
        implicit val bindTimeout: Timeout = Timeout(10.seconds)
        context.pipeToSelf(
          state.networkController.ask(NetworkController.ReceivableMessages.BindP2P).mapTo[Future[Tcp.Event]].flatten
        ) {
          case Success(Tcp.Bound(address)) =>
            ReceivableMessages.P2PTrafficBound(address)
          case Success(f: Tcp.CommandFailed) =>
            ReceivableMessages.Fail(f.cause.getOrElse(new IllegalArgumentException(f.toString())))
          case Success(f) =>
            ReceivableMessages.Fail(new IllegalArgumentException(f.toString))
          case Failure(exception) =>
            ReceivableMessages.Fail(exception)
        }
        Behaviors.same

      case (context, ReceivableMessages.P2PTrafficBound(p2pAddress)) =>
        context.log.info(s"${Console.YELLOW}P2P protocol bound to $p2pAddress${Console.RESET}")
        val service =
          prepareHttpService(
            settings,
            appContext,
            state.keyManager,
            state.forger,
            state.nodeViewHolder
          )(
            context.system
          )
        val httpHost = settings.rpcApi.bindAddress.getHostName
        val httpPort = settings.rpcApi.bindAddress.getPort

        /** trigger the HTTP server bind and check that the bind is successful. Terminate the application on failure */
        implicit val system: ActorSystem[_] = context.system
        context.pipeToSelf(Http().newServerAt(httpHost, httpPort).bind(service.compositeRoute)) {
          case Success(binding)   => ReceivableMessages.RPCTrafficBound(service, binding)
          case Failure(exception) => ReceivableMessages.Fail(exception)
        }
        Behaviors.same

      case (context, ReceivableMessages.RPCTrafficBound(_, binding)) =>
        context.log.info(s"${Console.YELLOW}HTTP server bound to ${binding.localAddress}${Console.RESET}")

        context.log.info("Bifrost initialized")
        running(State(state, binding))

      case (_, ReceivableMessages.Fail(reason)) =>
        throw reason
    }

  /**
   * The Heimdall state in which all of the child actors have been created, network traffic is bound, and
   * the application is in an operational state.
   */
  private def running(state: State): Behavior[ReceivableMessage] =
    Behaviors
      .receivePartial[ReceivableMessage] { case (_, ReceivableMessages.Fail(throwable)) =>
        throw throwable
      }
      .receiveSignal { case (_, PostStop) =>
        state.httpBinding.unbind()
        Behaviors.same
      }

  private case class KeyManagerInitializingState(
    keyManagerRef: CActorRef,
    futureKeyView: Future[StartupKeyView]
  )

  private case class NodeViewInitializingState(
    keyManager:     CActorRef,
    nodeViewHolder: ActorRef[NodeViewHolder.ReceivableMessage]
  )

  private case class NetworkControllerInitializingState(
    keyManager:        CActorRef,
    nodeViewHolder:    ActorRef[NodeViewHolder.ReceivableMessage],
    peerManager:       CActorRef,
    networkController: CActorRef,
    forger:            ActorRef[Forger.ReceivableMessage],
    mempoolAuditor:    ActorRef[MemPoolAuditor.ReceivableMessage]
  )

  private case class ActorsInitializedState(
    peerManager:          CActorRef,
    networkController:    CActorRef,
    keyManager:           CActorRef,
    forger:               ActorRef[Forger.ReceivableMessage],
    nodeViewHolder:       ActorRef[NodeViewHolder.ReceivableMessage],
    mempoolAuditor:       ActorRef[MemPoolAuditor.ReceivableMessage],
    peerSynchronizer:     CActorRef,
    nodeViewSynchronizer: CActorRef,
    chainReplicator:      Option[ActorRef[ChainReplicator.ReceivableMessage]]
  )

  private case class State(childActorState: ActorsInitializedState, httpBinding: Http.ServerBinding)

  private def prepareNodeViewState(
    settings: AppSettings
  )(implicit
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner,
    context:           ActorContext[ReceivableMessage],
    timeProvider:      TimeProvider
  ): NodeViewInitializingState = {
    import context.executionContext
    implicit def system: ActorSystem[_] = context.system
    implicit val getKeyViewAskTimeout: Timeout = Timeout(10.seconds)

    val keyManagerRef = context.actorOf(KeyManagerRef.props(settings), KeyManager.actorName)

    val nodeViewHolderRef =
      context.spawn(
        NodeViewHolder(
          settings,
          () =>
            NodeView.persistent(
              settings,
              () =>
                (keyManagerRef ? KeyManager.ReceivableMessages.GenerateInitialAddresses(
                  settings.forging.addressGenerationSettings
                ))
                  .mapTo[Try[StartupKeyView]]
                  .flatMap(Future.fromTry)
            )(context.system, implicitly, networkPrefix, protocolVersioner)
        ),
        NodeViewHolder.ActorName,
        DispatcherSelector.fromConfig("bifrost.application.node-view.dispatcher")
      )

    NodeViewInitializingState(keyManagerRef, nodeViewHolderRef)
  }

  private def prepareNetworkControllerState(
    settings:   AppSettings,
    appContext: AppContext,
    state:      NodeViewInitializingState
  )(implicit
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner,
    context:           ActorContext[ReceivableMessage],
    timeProvider:      TimeProvider
  ): NetworkControllerInitializingState = {

    implicit def system: ActorSystem[_] = context.system

    val peerManager =
      context.actorOf(PeerManagerRef.props(settings, appContext.externalNodeAddress), PeerManager.actorName)
    val networkController = context.actorOf(
      NetworkControllerRef.props(settings, peerManager, appContext, IO(Tcp)(context.system.toClassic))
    )
    val forgerRef = {
      implicit val timeout: Timeout = Timeout(10.seconds)
      context.spawn(
        Forger.behavior(
          settings.forging.blockGenerationDelay,
          settings.forging.minTransactionFee,
          settings.forging.forgeOnStartup,
          () => (state.keyManager ? KeyManager.ReceivableMessages.GetKeyView).mapTo[KeyView],
          new ActorNodeViewHolderInterface(state.nodeViewHolder)
        ),
        Forger.ActorName
      )
    }

    val mempoolAuditor =
      context.spawn(
        MemPoolAuditor(state.nodeViewHolder, networkController, settings),
        MemPoolAuditor.actorName
      )

    NetworkControllerInitializingState(
      state.keyManager,
      state.nodeViewHolder,
      peerManager,
      networkController,
      forgerRef,
      mempoolAuditor
    )
  }

  private def prepareRemainingActors(
    settings: AppSettings,
    state:    NetworkControllerInitializingState
  )(implicit
    context:      ActorContext[ReceivableMessage],
    timeProvider: TimeProvider
  ): ActorsInitializedState = {

    val peerSynchronizer = context.actorOf(
      PeerSynchronizerRef.props(state.networkController, state.peerManager, settings),
      PeerSynchronizer.actorName
    )

    val nodeViewSynchronizer = context.actorOf(
      NodeViewSynchronizerRef.props(state.networkController, state.nodeViewHolder, settings),
      NodeViewSynchronizer.actorName
    )

    val chainReplicator: Option[ActorRef[ChainReplicator.ReceivableMessage]] = {
      val chainRepSettings = settings.chainReplicator
      if (chainRepSettings.enableChainReplicator) {
        val dbOps =
          MongoDBOps(
            chainRepSettings.uri.getOrElse("mongodb://localhost"),
            chainRepSettings.database.getOrElse("bifrost")
          )

        Some(
          context.spawn(
            ChainReplicator(
              state.nodeViewHolder,
              dbOps,
              chainRepSettings
            ),
            ChainReplicator.actorName
          )
        )
      } else None
    }

    ActorsInitializedState(
      state.peerManager,
      state.networkController,
      state.keyManager,
      state.forger,
      state.nodeViewHolder,
      state.mempoolAuditor,
      peerSynchronizer,
      nodeViewSynchronizer,
      chainReplicator
    )
  }

  private def prepareHttpService(
    settings:          AppSettings,
    appContext:        AppContext,
    keyManagerRef:     CActorRef,
    forgerRef:         ActorRef[Forger.ReceivableMessage],
    nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit system:   ActorSystem[_]): HttpService = {
    import system.executionContext

    implicit val networkPrefix: NetworkPrefix =
      appContext.networkType.netPrefix

    implicit val protocolVersioner: ProtocolVersioner =
      ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)

    implicit val throwableEncoder: Encoder[ThrowableData] =
      ThrowableSupport.verbose(settings.rpcApi.verboseAPI)

    implicit val askTimeout: Timeout =
      Timeout(settings.rpcApi.timeout)

    val forgerInterface = new ActorForgerInterface(forgerRef)
    val keyManagerInterface = new ActorKeyManagerInterface(keyManagerRef)

    val nodeViewHolderInterface =
      new ActorNodeViewHolderInterface(nodeViewHolderRef)

    val bifrostRpcServer: ToplRpcServer = {
      import co.topl.rpc.handlers._
      new ToplRpcServer(
        ToplRpcHandlers(
          new DebugRpcHandlerImpls(nodeViewHolderInterface, keyManagerInterface),
          new UtilsRpcHandlerImpls,
          new NodeViewRpcHandlerImpls(settings.rpcApi, appContext, nodeViewHolderInterface),
          new TransactionRpcHandlerImpls(nodeViewHolderInterface),
          new AdminRpcHandlerImpls(forgerInterface, keyManagerInterface, nodeViewHolderInterface)
        ),
        appContext
      )
    }

    HttpService(settings.rpcApi, bifrostRpcServer)
  }

}
