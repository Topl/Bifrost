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

  override def munitTimeout: Duration = 15.minutes

  test("Multiple nodes launch and maintain consensus for three epochs") {
    val epochSlotLength: Long = 6 * 50 // See co.topl.node.ApplicationConfig.Bifrost.Protocol
    val bigBang = Instant.now().plusSeconds(30)
    val config0 = TestNodeConfig(bigBang, 3, 0, Nil)
    val config1 = TestNodeConfig(bigBang, 3, 1, List("MultiNodeTest-node0"))
    val config2 = TestNodeConfig(bigBang, 3, 2, List("MultiNodeTest-node1"))
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
        implicit0(dockerClient: DockerClient) = _dockerClient
        node1 <- dockerSupport.createNode("MultiNodeTest-node0", "MultiNodeTest", config0)
        node2 <- dockerSupport.createNode("MultiNodeTest-node1", "MultiNodeTest", config1)
        node3 <- dockerSupport.createNode("MultiNodeTest-node2", "MultiNodeTest", config2)
        nodes = List(node1, node2, node3)
        _ <- nodes.parTraverse(_.startContainer[F]).toResource
        _ <- nodes
          .parTraverse(node => node.rpcClient[F](node.config.rpcPort, tls = false).use(_.waitForRpcStartUp))
          .toResource
        _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
        thirdEpochHeads <- nodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort, tls = false)
              .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength * 3)).timeout(9.minutes).compile.lastOrError)
          )
          .toResource
        _ <- Logger[F].info("Nodes have reached target epoch").toResource
        heights = thirdEpochHeads.map(_.height)
        // All nodes should be at _roughly_ equal height
        _ <- IO(heights.max - heights.min <= 5).assert.toResource
        // All nodes should have a shared common ancestor near the tip of the chain
        _ <- nodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort, tls = false)
              .use(
                _.blockIdAtHeight(heights.min - 5)
              )
          )
          .map(_.toSet.size)
          .assertEquals(1)
          .toResource
      } yield ()

    resource.use_

  }
}
