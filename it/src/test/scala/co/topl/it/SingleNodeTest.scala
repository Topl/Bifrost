package co.topl.it

import cats.implicits._
import co.topl.akkahttprpc.implicits.client._
import co.topl.rpc.implicits.client._
import co.topl.it.util._
import co.topl.rpc.ToplRpc
import com.typesafe.config.ConfigFactory
import org.scalatest.EitherValues
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class SingleNodeTest extends AnyFreeSpec with Matchers with IntegrationSuite with ScalaFutures with EitherValues {

  "A single node can forge blocks" in {
    val nodeConfig =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = []
             |bifrost.rpcApi.namespaceSelector.debug = true
             |""".stripMargin
      )
    implicit val node: BifrostDockerNode =
      dockerSupport.createNode("bifrostTestNode", "SingleNodeTest")

    node.reconfigure(nodeConfig)

    node.start()

    node.waitForStartup().futureValue(Timeout(30.seconds)).value

    logger.info("Wait 2 seconds for forging")
    Thread.sleep(2.seconds.toMillis)

    val forgeCount1 =
      ToplRpc.Debug.MyBlocks.rpc.call.run(ToplRpc.Debug.MyBlocks.Params()).value.futureValue.value.count.toLong

    logger.info(s"Forge count=$forgeCount1")

    forgeCount1 should be > 0L

    logger.info("Wait 5 seconds for more forging")
    Thread.sleep(5.seconds.toMillis)

    val forgeCount2 =
      ToplRpc.Debug.MyBlocks.rpc.call.run(ToplRpc.Debug.MyBlocks.Params()).value.futureValue.value.count.toLong

    logger.info(s"Forge count=$forgeCount2")

    forgeCount2 should be > forgeCount1
  }

}
