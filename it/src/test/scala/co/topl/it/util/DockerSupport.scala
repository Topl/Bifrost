package co.topl.it.util

import akka.actor.ActorSystem
import co.topl.utils.Logging
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.ContainerConfig

import java.util.UUID

class DockerSupport(dockerClient: DockerClient)(implicit system: ActorSystem) extends Logging {

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString

  private val networkName = DockerSupport.networkNamePrefix + uuidShort

  private var nodeCache: Set[BifrostDockerNode] = Set.empty

  def createNode(name: String, peers: List[String]): BifrostDockerNode = {
    val containerName = networkName + "-" + name
    val containerConfig =
      buildContainerConfig(
        peers.zipWithIndex.map { case (peerAddress, index) =>
          s"BIFROST_KNOWN_PEERS.$index" -> peerAddress
        }.toMap
      )
    val containerCreation = dockerClient.createContainer(containerConfig, containerName)
    val node = BifrostDockerNode(containerCreation.id())
    nodeCache += node
    node
  }

  private def buildContainerConfig(environment: Map[String, String]): ContainerConfig =
    ContainerConfig
      .builder()
      .image(DockerSupport.bifrostImage)
      .exposedPorts(BifrostDockerNode.NetworkPort.toString, BifrostDockerNode.RpcPort.toString)
      .env(
        environment.toList.map { case (key, value) => s"$key=$value" }: _*
      )
      .build()

  def close(): Unit =
    nodeCache
      .map(_.containerId)
      .foreach(containerId => dockerClient.removeContainer(containerId, DockerClient.RemoveContainerParam.forceKill))

}

object DockerSupport {

  val bifrostImage: String = "bifrost:1.3.4"
  val networkNamePrefix: String = "bifrost-it"
}
