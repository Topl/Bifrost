package co.topl.tetra.it.util

import co.topl.buildinfo.node.BuildInfo
import co.topl.tetra.it.util.DockerSupport._
import co.topl.utils.Logging
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, NetworkConfig, NetworkCreation}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}

import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.jdk.CollectionConverters._

class DockerSupport(dockerClient: DockerClient) extends Logging {

  private var nodeCache: Set[BifrostDockerTetraNode] = Set.empty

  private var networkCache: Set[NetworkCreation] = Set.empty

  // @TODO required exposed ports could be read from config
  def createNode(
    name:          String,
    nodeGroupName: String,
    config:        Config = ConfigFactory.empty()
  ): BifrostDockerTetraNode = {
    val node = createContainer(name, nodeGroupName)
    configureContainer(node.containerId, config)
    node
  }

  private def createContainer(name: String, nodeGroupName: String): BifrostDockerTetraNode = {
    val networkName = DockerSupport.networkNamePrefix + nodeGroupName
    val containerConfig = buildContainerConfig(name, Map.empty)
    val containerCreation = dockerClient.createContainer(containerConfig, name)

    val node = BifrostDockerTetraNode(containerCreation.id())
    nodeCache += node

    val networkId =
      dockerClient.listNetworks().asScala.find(_.name == networkName) match {
        case Some(network) => network.id()
        case None =>
          val network =
            dockerClient.createNetwork(NetworkConfig.builder().name(networkName).build())
          networkCache += network
          network.id()
      }

    dockerClient.connectToNetwork(node.containerId, networkId)

    node
  }

  private def configureContainer(containerId: String, config: Config): Unit = {
    val configWithDefault = config.withFallback(DefaultConfig.config)
    val renderedConfig = configWithDefault.root().render(ConfigRenderOptions.concise())
    checkConfig(configWithDefault)

    logger.info(s"Reconfiguring to: $renderedConfig")
    require(!dockerClient.inspectContainer(containerId).state().running(), "Try to set configuration on running node")

    val tmpConfigDirectory = Files.createTempDirectory("bifrost-test-config")
    val testConfigPath = Paths.get(tmpConfigDirectory.toString, configFileName)
    Files.write(testConfigPath, renderedConfig.getBytes("UTF-8"))

    dockerClient.copyToContainer(tmpConfigDirectory, containerId, configDirectory)

    Files
      .walk(tmpConfigDirectory)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }

  private def checkConfig(configToCheck: Config): Unit =
    require(configToCheck.getString(rpcPortKeyPath) == rpcPort, "Custom RPC port is not supported now")

  private def buildContainerConfig(name: String, environment: Map[String, String]): ContainerConfig = {
    val env =
      environment.toList.map { case (key, value) => s"$key=$value" }

    val cmd =
      List(
        s"-Dcom.sun.management.jmxremote.port=$jmxRemotePort",
        s"-Dcom.sun.management.jmxremote.rmi.port=$jmxRemotePort",
        "-Dcom.sun.management.jmxremote.ssl=false",
        "-Dcom.sun.management.jmxremote.local.only=false",
        "-Dcom.sun.management.jmxremote.authenticate=false",
        "--config",
        s"$configDirectory/$configFileName",
        "--debug"
      )

    val hostConfig =
      HostConfig.builder().nanoCpus((1d * 1000000000).toLong).build()

    ContainerConfig
      .builder()
      .image(DockerSupport.bifrostImage)
      .env(env: _*)
      .volumes(configDirectory)
      .cmd(cmd: _*)
      .hostname(name)
      .hostConfig(hostConfig)
      .exposedPorts(exposedPorts: _*)
      .build()
  }

  def close(): Unit = {
    nodeCache
      .map(_.containerId)
      .foreach(containerId => dockerClient.removeContainer(containerId, DockerClient.RemoveContainerParam.forceKill))
    networkCache
      .map(_.id())
      .foreach(dockerClient.removeNetwork)
  }

}

object DefaultConfig {

  private val configMap = Map(
    "bifrost.rpc.bind-host" -> "0.0.0.0", // rpc will listen all interfaces instead of just localhost
    rpcPortKeyPath          -> rpcPort
  )

  val config: Config = ConfigFactory.parseMap(configMap.asJava)
}

object DockerSupport {
  val configDirectory = "/opt/docker/config"
  val configFileName = "testConfig.conf"

  val bifrostImage: String = s"toplprotocol/bifrost-node-tetra:${BuildInfo.version}"
  val networkNamePrefix: String = "bifrost-it-tetra"

  val rpcPortKeyPath = "bifrost.rpc.bind-port"
  val rpcPort = "9084"
  val jmxRemotePort = "9083"
  val exposedPorts: Seq[String] = List(rpcPort, jmxRemotePort)
}
