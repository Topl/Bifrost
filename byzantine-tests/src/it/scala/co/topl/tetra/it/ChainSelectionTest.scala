package co.topl.tetra.it

import cats.effect.implicits._
import cats.implicits._
import co.topl.tetra.it.util._
import com.spotify.docker.client.DockerClient
import java.time.Instant
import org.typelevel.log4cats.Logger
import scala.concurrent.duration._

/**
 * This test creates ~3 nodes that share the same seed but are not configured to talk to each other (meaning they
 * are siloed from one another).  After allowing each node to forge its own unique chain, the nodes are restarted with
 * communication enabled.  The nodes are expected to share the blocks they've produced by the other nodes, and each
 * node should independently agree that one chain is the best.
 */
class ChainSelectionTest extends IntegrationSuite {

  override def munitTimeout: Duration = 25.minutes

  test("Disconnected nodes can forge independently and later sync up to a proper chain") {
    val epochSlotLength = 500 // (50/4) * (100/15) * 6
    val bigBang = Instant.now().plusSeconds(30)
    val config0 = DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 0, knownPeers = Nil)
    val config1 = DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 1, knownPeers = Nil)
    val config2 = DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 2, knownPeers = Nil)

    val nodesWithknownPeers = List(
      DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 1, knownPeers = Nil),
      DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 1, knownPeers = List("ChainSelectionTest-node0")),
      DefaultConfig(bigBang, stakerCount = 3, localStakerIndex = 2, knownPeers = List("ChainSelectionTest-node1"))
    )

    val resource = for {
      (dockerSupport, _dockerClient) <- DockerSupport.make[F]
      implicit0(dockerClient: DockerClient) = _dockerClient
      node1 <- dockerSupport.createNode("ChainSelectionTest-node0", "ChainSelectionTest", config0)
      node2 <- dockerSupport.createNode("ChainSelectionTest-node1", "ChainSelectionTest", config1)
      node3 <- dockerSupport.createNode("ChainSelectionTest-node2", "ChainSelectionTest", config2)
      
      nodes = List(node1, node2, node3)
      _ <- nodes.parTraverse(_.startContainer[F]).toResource
      _ <- nodes.parTraverse(_.rpcClient[F].use(_.waitForRpcStartUp)).toResource
      _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
      blockHeaders <- nodes
        .parTraverse(node =>
          node
            .rpcClient[F]
            .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength)).timeout(15.minutes).compile.lastOrError)
        )
        .toResource

      _ <- blockHeaders.size.pure[F].assertEquals(3).toResource
      _ <- blockHeaders.distinct.size.pure[F].assertEquals(3).toResource

      // Stop, configure with KnownPeers, and run again
      _ <- nodes.parTraverse(_.stop[F]).toResource
      _ <- nodes.zip(nodesWithknownPeers).parTraverse { case (node, config) => node.configure[F](config) }.toResource
      _ <- nodes.parTraverse(_.restartContainer[F]).toResource
      _ <- nodes.parTraverse(_.rpcClient[F].use(_.waitForRpcStartUp)).toResource
      _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
      thirdEpochHeads <- nodes
        .parTraverse(
          _.rpcClient[F]
            .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength * 3)).timeout(20.minutes).compile.lastOrError)
        )
        .toResource
      _ <- Logger[F].info("Nodes have reached target epoch").toResource
      heights = thirdEpochHeads.map(_.height)
      // All nodes should be at _roughly_ equal height
      _ <- Logger[F].info(heights.mkString(",")).toResource
      _ <- (heights.max - heights.min <= 10).pure[F].assert.toResource
      // All nodes should have a shared common ancestor near the tip of the chain
      _ <- nodes
        .parTraverse(
          _.rpcClient[F].use(
            _.blockIdAtHeight(heights.min - 10)
          )
        )
        .map(_.toSet.size)
        .assertEquals(1)
        .toResource

    } yield ()

    resource.use_
  }

}
