package co.topl.it.util

import java.util.UUID

import co.topl.settings.AppSettings
import co.topl.utils.Logging
import co.topl.settings.AppSettings
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{Container, ContainerConfig}
import com.typesafe.config.{Config, ConfigFactory}
import org.asynchttpclient.Dsl.{asyncHttpClient, config}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.control.NonFatal

class Docker()(implicit ec: ExecutionContext) extends AutoCloseable with Logging {

  private val http = asyncHttpClient(
    config()
      .setMaxConnections(18)
      .setMaxConnectionsPerHost(3)
      .setMaxRequestRetry(1)
      .setReadTimeout(10000)
      .setKeepAlive(false)
      .setRequestTimeout(10000)
  )

  private val client = DefaultDockerClient.fromEnv().build()
  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.debug("Shutdown hook")
    close()
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    log.debug(s"Starting ${nodeConfigs.size} containers")
    val nodes = nodeConfigs.map(startNode)
    Await.result(
      Future.traverse(nodes)(_.waitForStartup),
      5.minutes
    )
    nodes
  }

  private def startNode(nodeConfig: Config): Node = {
    try {
      val settings = buildAppSettings(nodeConfig)
      val containerConfig: ContainerConfig = buildContainerConfig(settings)
      val containerName: String = networkName + "-" + settings.network.nodeName
      val containerId: String = client.createContainer(containerConfig, containerName).id
      Node(settings, containerId, 9084, 9085)
    } catch {
      case NonFatal(e) => throw e
    }
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] =
    Future.sequence(nodes map { _.waitForStartup })

  private def buildAppSettings(nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    AppSettings.fromConfig(actualConfig)
  }

  private def buildContainerConfig(settings: AppSettings): ContainerConfig = {

    val shellCmd = "echo Options: $OPTS; java $OPTS -jar /usr/src/bifrost/bifrost.jar"

    ContainerConfig
      .builder()
      .image(Docker.bifrostImage)
      .exposedPorts(settings.network.bindAddress.getPort.toString, settings.restApi.bindAddress.getPort.toString)
      .entrypoint("sh", "-c", shellCmd)
      .build()
  }

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x =
      if (containers.isEmpty) "No"
      else
        "\n" + containers.asScala
          .map { x =>
            s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})"
          }
          .mkString("\n")

    log.debug(s"$label: $x")
  }

  override def close(): Unit = {}
}

object Docker {

  val bifrostImage: String = "toplprotocol/bifrost:latest"
  val networkNamePrefix: String = "bifrost-it"
}
