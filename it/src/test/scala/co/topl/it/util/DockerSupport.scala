package co.topl.it.util

import akka.actor.ActorSystem
import co.topl.buildinfo.bifrost.BuildInfo
import co.topl.it.api.NodeRpcApi
import co.topl.utils.Logging
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.ContainerConfig

import java.util.UUID

class DockerSupport(dockerClient: DockerClient)(implicit system: ActorSystem) extends Logging {

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString

  private val networkName = DockerSupport.networkNamePrefix + uuidShort

  private var nodeCache: Set[BifrostDockerNode] = Set.empty

  def createNode(name: String): BifrostDockerNode = {
    val containerName = networkName + "-" + name
    val containerConfig = buildContainerConfig(Map.empty, containerName)
    val containerCreation = dockerClient.createContainer(containerConfig, containerName)

    val node = BifrostDockerNode(containerCreation.id())
    nodeCache += node
    node
  }

  private def buildContainerConfig(environment: Map[String, String], seed: String): ContainerConfig = {
    val env =
      environment.toList.map { case (key, value) => s"$key=$value" }

    val cmd =
      List("--apiKeyHash", NodeRpcApi.ApiKeyHashBase58, "-c", "/opt/docker/config/testConfig.conf", "-s", seed)

    ContainerConfig
      .builder()
      .image(DockerSupport.bifrostImage)
      .env(env: _*)
      .volumes("/opt/docker/config")
      .cmd(cmd: _*)
      .build()

  }

  def close(): Unit =
    nodeCache
      .map(_.containerId)
      .foreach(containerId => dockerClient.removeContainer(containerId, DockerClient.RemoveContainerParam.forceKill))

}

object DockerSupport {

  val bifrostImage: String = s"bifrost:${BuildInfo.version}"
  val networkNamePrefix: String = "bifrost-it"
}
