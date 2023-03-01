package co.topl.tetra.it

import cats.effect.implicits._
import co.topl.tetra.it.util._
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger

class SanityCheckNodeTest extends IntegrationSuite {

  test("A single node is successfully started, id of the genesis block is available through RPC") {
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]
        implicit0(dockerClient: DockerClient) = _dockerClient
        node1       <- dockerSupport.createNode("SingleNodeTest-node1", "SingleNodeTest", DefaultConfig())
        _           <- node1.startContainer[F].toResource
        node1Client <- node1.rpcClient[F]
        _           <- node1Client.waitForRpcStartUp.toResource
        _           <- Logger[F].info("Fetching genesis block").toResource
        _           <- node1Client.blockIdAtHeight(1).map(_.nonEmpty).assert.toResource
        _           <- Logger[F].info("Success").toResource
      } yield ()

    resource.use_
  }
}
