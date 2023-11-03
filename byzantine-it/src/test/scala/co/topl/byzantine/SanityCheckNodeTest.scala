package co.topl.byzantine

import co.topl.byzantine.util._
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger
import co.topl.interpreters.NodeRpcOps._
import scala.concurrent.duration._

class SanityCheckNodeTest extends IntegrationSuite {

  test("A single node is successfully started, id of the genesis block is available through RPC") {
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
        implicit0(dockerClient: DockerClient) = _dockerClient
        node1 <- dockerSupport.createNode("SingleNodeTest-node1", "SingleNodeTest", TestNodeConfig(genusEnabled = true))
        _     <- node1.startContainer[F].toResource
        node1Client  <- node1.rpcClient[F](node1.config.rpcPort, tls = false)
        genus1Client <- node1.rpcGenusClient[F](node1.config.rpcPort, tls = false)
        _            <- node1Client.waitForRpcStartUp.toResource
        _            <- genus1Client.waitForRpcStartUp.toResource
        _            <- Logger[F].info("Fetching genesis block Node Grpc Client").toResource
        _            <- node1Client.blockIdAtHeight(1).map(_.nonEmpty).assert.toResource
        _            <- Logger[F].info("Fetching genesis block Genus Grpc Client").toResource
        _            <- genus1Client.blockIdAtHeight(1).map(_.block.header.height).assertEquals(1L).toResource
        // Restart the container to verify that it is able to reload from disk
        _ <- node1.restartContainer[F].toResource
        _ <- node1Client.waitForRpcStartUp.toResource
        _ <- fs2.Stream
          .force(node1Client.synchronizationTraversal())
          .drop(1)
          .head
          .compile
          .lastOrError
          .timeout(1.minute)
          .toResource
        _ <- Logger[F].info("Success").toResource
      } yield ()

    resource.use_
  }
}
