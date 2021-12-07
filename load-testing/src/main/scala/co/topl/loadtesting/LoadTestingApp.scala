package co.topl.loadtesting

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import co.topl.client.Provider
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType._
import mainargs.{arg, main, ParserForMethods, TokensReader}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/**
 * Root app object for the Load Testing App.
 */
object LoadTestingApp {

  implicit object networkReader
      extends TokensReader[NetworkType](
        shortName = "network",
        str =>
          NetworkType.pickNetworkType(str.head) match {
            case Some(net) => Right(net)
            case None      => Left("No valid network found with that name")
          }
      )

  /**
   * Starts a load test with the configured parameters.
   * @param network the Bifrost network type to connect to
   * @param userCount the number of users to generate
   * @param uri a Bifrost node to connect to
   * @param apiKey the Bifrost node's API key
   * @param seed the user address generation seed
   * @param outputDirectory a path to a directory for results output
   * @param timeoutSeconds a general akka timeout (fix)
   */
  @main
  def run(
    @arg(short = 'n') network:         NetworkType,
    @arg(short = 'c') userCount:       Int,
    @arg(short = 'u') uri:             String,
    @arg(short = 'a') apiKey:          String,
    @arg(short = 's') seed:            String = "test",
    @arg(short = 'o') outputDirectory: String = "./stats",
    @arg(short = 't') timeoutSeconds:  Int = 30
  ): Unit = {
    val provider: Provider = network match {
      case PrivateTestnet  => new Provider.PrivateTestNet(uri, apiKey)
      case ValhallaTestnet => new Provider.ValhallaTestNet(uri, apiKey)
      case _               => throw new Error("invalid testnet: not supported")
    }

    import provider._

    implicit val timeout: Timeout = timeoutSeconds.seconds

    implicit val actorSystem: ActorSystem[SimulationActor.Command] =
      ActorSystem[SimulationActor.Command](SimulationActor(outputDirectory), "simulation")

    actorSystem ! SimulationActor.AddUsers(userCount, seed)

    def shutdown(): Unit = {
      actorSystem ! SimulationActor.Stop
      actorSystem.terminate()

      Await.result(actorSystem.whenTerminated, 1.minute)
    }

    sys.addShutdownHook(shutdown())
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
