package bifrost.scorexMod

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import bifrost.PersistentNodeViewModifier
import bifrost.api.http.{ApiRoute, CompositeHttpService}
import bifrost.network.message._
import bifrost.network.peer.PeerManager
import bifrost.network.{NetworkController, UPnP}
import bifrost.settings.Settings
import bifrost.transaction.box.proposition.Proposition
import bifrost.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type

trait GenericApplication extends ScorexLogging {

  type P <: Proposition
  type TX <: GenericBoxTransaction[P, _, BX]
  type BX <: GenericBox[P, _]
  type PMOD <: PersistentNodeViewModifier[P, TX]
  type NVHT <: GenericNodeViewHolder[_, P, TX, BX, PMOD]

  val ApplicationNameLimit = 50

  //settings
  implicit val settings: Settings

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem(settings.agentName)

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  lazy val upnp = new UPnP(settings)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  val nodeViewHolderRef: ActorRef
  val nodeViewSynchronizer: ActorRef
  val localInterface: ActorRef


  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController = actorSystem.actorOf(nProps, "networkController")

  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute

  def run(): Unit = {
    require(settings.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}