package co.topl.it

import co.topl.it.util._
import co.topl.rpc.ToplRpc
import co.topl.utils.Int128
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.syntax._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Ignore, Inspectors}

import scala.concurrent.Future
import scala.concurrent.duration._

// NOTE: This test currently fails because block difficulties diverge between nodes.  When nodes re-join, the blocks
// can't be properly appended.
@Ignore
class ChainSelectionTest
    extends AnyFreeSpec
    with Matchers
    with IntegrationSuite
    with ScalaFutures
    with EitherValues
    with Inspectors {

  // TODO: This shouldn't be hard-coded
  private val maxStake = 200000000000000000L

  val nodeCount: Int = 4
  val initialForgeCountTarget: Int128 = 5
  val syncedForgeDuration: FiniteDuration = 10.seconds
  val seed: String = "ChainSelectionTest" + System.currentTimeMillis()

  val nodeNames: List[String] = List.tabulate(nodeCount)("bifrostMultiNode" + _)

  val baseConfig: Config =
    ConfigFactory.parseString(
      raw"""bifrost.network.knownPeers = []
           |bifrost.rpcApi.namespaceSelector.debug = true
           |bifrost.forging.privateTestnet.numTestnetAccts = $nodeCount
           |bifrost.forging.privateTestnet.testnetBalance = ${maxStake / nodeCount}
           |bifrost.forging.privateTestnet.genesisSeed = "$seed"
           |bifrost.forging.forgeOnStartup = false
           |""".stripMargin
    )

  val connectedConfig: Config =
    ConfigFactory
      .parseString(
        raw"""bifrost.network.knownPeers = ${nodeNames.map(_ + ":" + BifrostDockerNode.NetworkPort).asJson.noSpaces}
           |""".stripMargin
      )
      .withFallback(baseConfig)

  "Disconnected nodes can forge independently and later sync up to a proper chain" in {
    val nodes = nodeNames.map(dockerSupport.createNode(_, "ChainSelectionTest"))
    nodes.foreach(_.reconfigure(baseConfig))
    nodes.foreach(_.start())
    Future
      .traverse(nodes)(_.waitForStartup().map(_.value))
      .futureValue(Timeout(60.seconds))
    assignForgingAddress(nodes)

    logger.info(s"Waiting for each node to forge $initialForgeCountTarget blocks")
    nodes.foreach(_.run(ToplRpc.Admin.StartForging.rpc)(ToplRpc.Admin.StartForging.Params()).value)
    Future
      .traverse(nodes)(node =>
        node
          .pollUntilHeight(initialForgeCountTarget)
          .map(_.value)
          .map(_ => node.run(ToplRpc.Admin.StopForging.rpc)(ToplRpc.Admin.StopForging.Params()).value)
      )
      .futureValue(Timeout(10.minutes))

    logger.info("Restarting nodes with connected config")
    nodes.foreach(_.reconfigure(connectedConfig))
    Thread.sleep(5.seconds.toMillis)
    Future
      .traverse(nodes)(_.waitForStartup().map(_.value))
      .futureValue(Timeout(60.seconds))

    logger.info(s"Allowing blocks to forge and sync for $syncedForgeDuration")
    nodes.foreach(_.run(ToplRpc.Admin.StartForging.rpc)(ToplRpc.Admin.StartForging.Params()).value)
    Thread.sleep(syncedForgeDuration.toMillis)

    logger.info("Stopping forging and allowing nodes to sync for a bit longer")
    nodes.foreach(_.run(ToplRpc.Admin.StopForging.rpc)(ToplRpc.Admin.StopForging.Params()).value)
    Thread.sleep(30.seconds.toMillis)

    logger.info("Comparing node heads")

    val bestBlocks = nodes.map(_.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value)

    logger.info(s"Best blocks: $bestBlocks")

    bestBlocks.map(_.height).toSet should have size 1
    bestBlocks.map(_.bestBlockId).toSet should have size 1
  }

}
