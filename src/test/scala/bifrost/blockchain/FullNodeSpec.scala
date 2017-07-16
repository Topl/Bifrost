package bifrost.blockchain

import akka.actor.{Actor, ActorKilledException, ActorLogging, ActorRef, ActorSystem, Kill, Props}
import akka.event.LoggingAdapter
import akka.testkit.{DebugFilter, EventFilter, ImplicitSender, TestActorRef, TestActors, TestKit}
import bifrost.{BifrostApp, BifrostLocalInterface, BifrostNodeViewHolder}
import bifrost.blockchain.TeacherProtocol.QuoteRequest
import bifrost.forging.Forger.{StartForging, StopForging}
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.{BifrostNodeViewSynchronizer, ProducerNotifySpec}
import bifrost.scorexMod.GenericApplication
import bifrost.scorexMod.GenericNodeViewSynchronizer.{CompareViews, RequestFromLocal}
import io.circe
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import scorex.core.network.{NetworkController, UPnP}
import scorex.core.network.message._
import scorex.core.network.peer.PeerManager
import scorex.core.utils.ScorexLogging

import scala.reflect.io.Path
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

object TeacherProtocol{

  case class QuoteRequest()
  case class QuoteResponse(quoteString:String)

}
class TeacherLogActor extends Actor with ScorexLogging {
  import TeacherProtocol._

  val quotes = List(
    "Moderation is for cowards",
    "Anything worth doing is worth overdoing",
    "The trouble is you think you have time",
    "You never gonna know if you never even try")

  def receive = {

    case QuoteRequest => {

      //Get a random Quote from the list and construct a response
      val quoteResponse=QuoteResponse(quotes(Random.nextInt(quotes.size)))
      log.info(quoteResponse.toString)
    }
  }

  def quoteList=quotes
}

class FullNodeSpec extends TestKit(ActorSystem("TestNode"))
  with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {
  import FullNodeSpec._
  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

//  val localInterface0 = tempNode0.localInterface

//  val nodeViewHolder1 = tempNode1.nodeViewHolderRef
//  val forger1 = tempNode1.forger
//  val localInterface1 = tempNode1.localInterface

  var nodeViewHolder0 = ActorRef.noSender
  var forger0 = ActorRef.noSender

  override def beforeAll: Unit = {
    val path: Path = Path ("/tmp/scorex/data")
    Try(path.deleteRecursively())
    nodeViewHolder0 = system.actorOf(Props(new BifrostNodeViewHolder(settings)))
    forger0 = system.actorOf(Props(classOf[Forger], settings, nodeViewHolder0))
  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A NodeViewHolderActor" must {

    "Not Get trying to generate message after Stop Forging message is sent" in {
      forger0
      Thread.sleep(4000)
      Try {
        EventFilter.info(pattern = "Trying to generate.*", occurrences = 1) intercept {
          forger0 ! StopForging
          Thread.sleep(7000)
        }
      } match {
        case Failure(e) => e.getMessage shouldBe "timeout (3 seconds) waiting for 1 messages on InfoFilter(None,Right(Trying to generate.*),false)"
      }
    }
  }
}

object FullNodeSpec {
  //val tempNode0 = new BifrostApp("settings.json")
  // val tempNode1 = new BifrostApp("settings2.json")
}