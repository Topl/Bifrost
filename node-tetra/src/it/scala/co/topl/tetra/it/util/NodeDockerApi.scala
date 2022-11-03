package co.topl.tetra.it.util

import com.spotify.docker.client.DockerClient
import org.slf4j.{Logger, LoggerFactory}

case class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  private val timeout = 10
  val logger: Logger = LoggerFactory.getLogger(this.toString)

  def start(): Unit = {
    logger.info(s"Starting")
    dockerClient.startContainer(containerId)
    awaitContainerStart()
    logger.info(s"Successfully started container $containerId on IP ${ipAddress()}")
  }

  def stop(): Unit = {
    logger.info(s"Stopping")
    dockerClient.stopContainer(containerId, timeout)
  }

  def ipAddress(): String =
    dockerClient.inspectContainer(containerId).networkSettings().ipAddress()

  private def awaitContainerStart(): Unit = {
    var remainingAttempts = 10
    var ip = ipAddress()
    while (ip.isEmpty && remainingAttempts > 0) {
      Thread.sleep(1000)
      ip = ipAddress()
      remainingAttempts -= 1
    }
    if (ip.isEmpty) throw new IllegalStateException(s"Container IP not found. ContainerId=$containerId")
  }

}

object NodeDockerApi {

  def apply(node: BifrostDockerTetraNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    new NodeDockerApi(node.containerId)

}
