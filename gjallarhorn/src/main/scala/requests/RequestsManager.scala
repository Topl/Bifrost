package requests

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
 * Akka actor that manages request from Gjallarhorn to Bifrost (WalletConnectionHandler)
 * @param bifrostActorRef the actor ref for Bifrost's WalletConnectionHandler
 * @param ec the execution context used for Futures.
 */
class RequestsManager(val bifrostActorRef: ActorRef)(implicit ec: ExecutionContext) extends Actor with Logging {

  import RequestsManager._

  implicit val timeout: Timeout = 30.seconds

  override def receive: Receive = {

    /** Sends given request to bifrost and sends future response back to sender on complete */
    case BifrostRequest(tx: Json) =>
      val futureResponse = bifrostActorRef ? s"request from gjallarhorn: $tx"
      futureResponse.pipeTo(sender())

  }
}

object RequestsManager {
  case class BifrostRequest(tx: Json)
}
