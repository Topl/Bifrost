package co.topl.it

import co.topl.it.api.{NodeDockerApi, NodeRpcApi}
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
    val node = dockerSupport.createNode("bifrostTestNode")

    NodeDockerApi(node).reconfigure(nodeConfig)

    NodeDockerApi(node).start()

    NodeRpcApi(node).waitForStartup().futureValue(Timeout(30.seconds))

    val forgeCount1 =
      NodeRpcApi(node).debug.myBlocks().futureValue.value

    Thread.sleep(20.seconds.toMillis)

    val forgeCount2 =
      NodeRpcApi(node).debug.myBlocks().futureValue.value

    forgeCount2 should be > forgeCount1
  }

}
