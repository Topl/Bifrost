package co.topl.tetra.it

import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._
import co.topl.tetra.it.util._
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.concurrent.duration._

class MultiNodeTest extends IntegrationSuite {

  override def munitTimeout: Duration = 30.minutes

  test("Multiple nodes launch and maintain consensus for three epochs") {
    val epochSlotLength = 500 // (50/4) * (100/15) * 6
    val bigBang = Instant.now().plusSeconds(15)
    val config0 = DefaultConfig(bigBang, 3, 0, List("MultiNodeTest-node2"))
    val config1 = DefaultConfig(bigBang, 3, 1, List("MultiNodeTest-node0"))
    val config2 = DefaultConfig(bigBang, 3, 1, List("MultiNodeTest-node1"))
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]
        implicit0(dockerClient: DockerClient) = _dockerClient
        node1 <- dockerSupport.createNode("MultiNodeTest-node0", "MultiNodeTest", config0)
        node2 <- dockerSupport.createNode("MultiNodeTest-node1", "MultiNodeTest", config1)
        node3 <- dockerSupport.createNode("MultiNodeTest-node2", "MultiNodeTest", config2)
        nodes = List(node1, node2, node3)
        _ <- nodes.parTraverse { node =>
          node.startContainer[F] >>
          node.rpcClient[F].use(_.waitForRpcStartUp)
        }.toResource
        _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
        thirdEpochHeads <- nodes
          .parTraverse(
            _.rpcClient[F]
              .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength * 3)).timeout(12.minutes).compile.lastOrError)
          )
          .toResource
        _ <- Logger[F].info("Nodes have reached target epoch").toResource
        heights = thirdEpochHeads.map(_.height)
        _ <- IO(heights.max - heights.min <= 5).assert.toResource // All nodes should be at _roughly_ equal height
        _ <- nodes
          .parTraverse(
            _.rpcClient[F].use(
              _.blockIdAtHeight(heights.min - 5)
            )
          )
          .map(_.toSet.size)
          .assertEquals(1)
          .toResource // All nodes should have a shared common ancestor near the tip of the chain
      } yield ()

    resource.use_

  }
}
