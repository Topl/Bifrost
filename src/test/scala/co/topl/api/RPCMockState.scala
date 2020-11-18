package co.topl.api

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.Timeout
import co.topl.consensus.{Forger, ForgerRef}
import co.topl.nodeView.NodeViewHolderRef
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.DurationInt

trait RPCMockState extends AnyWordSpec
  with CoreGenerators
  with ScalatestRouteTest {

  override def createActorSystem(): ActorSystem = ActorSystem(settings.network.agentName)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val appContext = new AppContext(settings, StartupOpts.empty, None)

  // Create Bifrost singleton actors
  protected val forgerRef: ActorRef = ForgerRef(Forger.actorName, settings, appContext)
  protected val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  implicit val timeout: Timeout = Timeout(10.seconds)
}
