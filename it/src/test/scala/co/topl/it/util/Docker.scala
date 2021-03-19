package co.topl.it.util

import akka.actor.ActorSystem
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{Container, ContainerConfig}
import com.typesafe.config.{Config, ConfigFactory}

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.asScalaBufferConverter

class Docker()(implicit ec: ExecutionContext, system: ActorSystem) extends AutoCloseable with Logging {

  private implicit val client: DefaultDockerClient = DefaultDockerClient.fromEnv().build()
  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.info("Shutdown hook")
    close()
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    log.info(s"Starting ${nodeConfigs.size} containers")
    val nodes = nodeConfigs.map(startNode)
    Await.result(
      Future.traverse(nodes)(_.waitForStartup),
      5.minutes
    )
    nodes
  }

  private def startNode(nodeConfig: Config): Node = {
    val settings = buildAppSettings(nodeConfig)
    val containerConfig: ContainerConfig = buildContainerConfig(settings)
    val containerName: String = networkName + "-" + settings.network.nodeName
    val containerId: String = client.createContainer(containerConfig, containerName).id
    client.startContainer(containerId)
    Node(settings, containerId, 9084, 9085)
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

  private def buildContainerConfig(settings: AppSettings): ContainerConfig =
    ContainerConfig
      .builder()
      .image(Docker.bifrostImage)
      .exposedPorts(settings.network.bindAddress.getPort.toString, settings.rpcApi.bindAddress.getPort.toString)
      .build()

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x =
      if (containers.isEmpty) "No"
      else
        "\n" + containers.asScala
          .map { x =>
            s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})"
          }
          .mkString("\n")

    log.info(s"$label: $x")
  }

  override def close(): Unit = {}
}

object Docker {

  val bifrostImage: String = "bifrost:1.3.4"
  val networkNamePrefix: String = "bifrost-it"
}
