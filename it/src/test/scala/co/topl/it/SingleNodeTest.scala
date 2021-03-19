package co.topl.it

import co.topl.it.api.{NodeDockerApi, NodeRpcApi}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.duration._

class SingleNodeTest extends AnyFreeSpec with IntegrationSuite with ScalaFutures {

  "A single node can forge blocks" in {
    val node = dockerSupport.createNode("bifrostTestNode", Nil)

    NodeDockerApi(node).start()

    NodeRpcApi(node).waitForStartup().futureValue(Timeout(30.seconds))
  }

}
