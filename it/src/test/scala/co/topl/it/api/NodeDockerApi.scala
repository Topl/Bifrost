package co.topl.it.api

import co.topl.it.util.BifrostDockerNode
import com.spotify.docker.client.DockerClient

class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  def start(): Unit =
    dockerClient.startContainer(containerId)

  def stop(): Unit =
    dockerClient.stopContainer(containerId, 10)

  def ipAddress(): String =
    dockerClient.inspectContainer(containerId).networkSettings().ipAddress()

}

object NodeDockerApi {
  def apply(node: BifrostDockerNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}