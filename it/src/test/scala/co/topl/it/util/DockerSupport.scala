package co.topl.it.util

import akka.actor.ActorSystem
import co.topl.buildinfo.bifrost.BuildInfo
import co.topl.utils.Logging
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.ContainerConfig

class DockerSupport(dockerClient: DockerClient)(implicit system: ActorSystem) extends Logging {

  private var nodeCache: Set[BifrostDockerNode] = Set.empty

  def createNode(name: String): BifrostDockerNode = {
    val containerConfig = buildContainerConfig(name, Map.empty, name)
    val containerCreation = dockerClient.createContainer(containerConfig, name)

    val node = BifrostDockerNode(containerCreation.id())
    nodeCache += node
    node
  }

  private def buildContainerConfig(name: String, environment: Map[String, String], seed: String): ContainerConfig = {
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
      .hostname(name)
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
