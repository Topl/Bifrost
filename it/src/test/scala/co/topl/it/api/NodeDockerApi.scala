package co.topl.it.api

import co.topl.it.util.BifrostDockerNode
import com.spotify.docker.client.DockerClient
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import io.circe.syntax._

import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.collection.JavaConverters._

class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  def start(): Unit =
    dockerClient.startContainer(containerId)

  def stop(): Unit =
    dockerClient.stopContainer(containerId, 10)

  def ipAddress(): String =
    dockerClient.inspectContainer(containerId).networkSettings().ipAddress()

  def reconfigure(config: Config): Unit = {
    val wasRunning = dockerClient.inspectContainer(containerId).state().running()
    if (wasRunning) {
      stop()
    }

    val tmpConfigDirectory = Files.createTempDirectory("bifrost-test-config")
    val renderedConfig = config.root().render(ConfigRenderOptions.concise())
    val testConfigPath = Paths.get(tmpConfigDirectory.toString, "testConfig.conf")
    Files.write(testConfigPath, renderedConfig.getBytes)

    dockerClient.copyToContainer(tmpConfigDirectory, containerId, "/opt/docker/config")

    Files
      .walk(tmpConfigDirectory)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)

    if (wasRunning) {
      start()
    }
  }

  def reconfigure(knownPeers: List[String]): Unit = {
    val nodeConfig =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = ${knownPeers.asJson}
             |bifrost.rpcApi.namespaceSelector.debug = true
             |""".stripMargin
      )

    reconfigure(nodeConfig)
  }

}

object NodeDockerApi {

  def apply(node: BifrostDockerNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}
