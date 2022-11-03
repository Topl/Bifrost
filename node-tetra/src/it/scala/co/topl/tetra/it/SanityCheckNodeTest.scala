package co.topl.tetra.it

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.grpc.ToplGrpc
import co.topl.tetra.it.util._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SanityCheckNodeTest extends AnyFreeSpec with Matchers with IntegrationSuite {

  "A single node is successfully started, id of the genesis block is available through RPC" in {
    val node: BifrostDockerTetraNode =
      dockerSupport.createNode("node-tetra", "SingleNodeTest")

    node.start()
    node.waitForRpcStartUp()

    val genesisBlockIdOpt =
      ToplGrpc.Client
        .make[IO](node.host, node.rpcPort, tls = false)
        .use { rpc =>
          rpc.blockIdAtHeight(1)
        }
        .unsafeRunSync()

    assert(genesisBlockIdOpt.isDefined, "Successfully got genesis block id")
  }
}
