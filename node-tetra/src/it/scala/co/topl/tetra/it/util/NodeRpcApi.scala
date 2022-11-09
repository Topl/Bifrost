package co.topl.tetra.it.util

import cats.Monad
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeError
import co.topl.algebras.ToplRpc
import co.topl.grpc.ToplGrpc
import co.topl.tetra.it.util.NodeRpcApi.{rpcWaitAttempts, rpcWaitSleepMs}
import com.spotify.docker.client.DockerClient
import org.slf4j.{Logger, LoggerFactory}
import fs2.Stream

case class NodeRpcApi(host: String, rpcPort: Int) {
  val logger: Logger = LoggerFactory.getLogger(this.toString)

  def waitForRpcStartUp(): Unit = {
    logger.info(s"Start waiting of RPC startup for host $host")

    ToplGrpc.Client
      .make[IO](host, rpcPort, tls = false)
      .use(waitRpcIO)
      .unsafeRunSync()

    logger.info(s"RPC is started and run for host $host")
  }

  private def waitRpcIO(rpc: ToplRpc[IO, Stream[IO, *]]): IO[Unit] = {
    var remainingAttempts = rpcWaitAttempts

    def sleepWithCounter = IO {
      if (remainingAttempts > 0) {
        Thread.sleep(rpcWaitSleepMs)
        remainingAttempts -= 1
      } else {
        throw new IllegalStateException(s"Failed to connect RPC on $host:$rpcPort")
      }
    }

    def rpcIsAvailable =
      rpc.blockIdAtHeight(1).recover(_ => None).map(_.isDefined)

    Monad[IO].untilM_(sleepWithCounter)(rpcIsAvailable)
  }
}

object NodeRpcApi {
  val rpcWaitAttempts = 30
  val rpcWaitSleepMs = 1000

  def apply(node: BifrostDockerTetraNode)(implicit dockerClient: DockerClient): NodeRpcApi = {
    val host = dockerClient.inspectContainer(node.containerId).networkSettings().ipAddress()
    require(host.nonEmpty, s"Docker container is missing IP address. ContainerId=${node.containerId}")
    new NodeRpcApi(host, DockerSupport.rpcPort.toInt)
  }
}
