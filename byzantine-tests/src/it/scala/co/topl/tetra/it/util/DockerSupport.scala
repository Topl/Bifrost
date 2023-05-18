package co.topl.tetra.it.util

import cats.Applicative
import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import co.topl.buildinfo.node.BuildInfo
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.messages.HostConfig
import com.spotify.docker.client.messages.NetworkConfig
import com.spotify.docker.client.messages.NetworkCreation
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient
import fs2.io.file.Files
import fs2.io.file.Path

import java.time.Instant
import scala.jdk.CollectionConverters._

trait DockerSupport[F[_]] {

  def createNode(
    name:          String,
    nodeGroupName: String,
    config:        TestNodeConfig
  ): Resource[F, BifrostDockerTetraNode]
}

object DockerSupport {

  private def loggingEnabledFromEnvironment: Boolean = {
    import scala.jdk.CollectionConverters._
    val env = System.getenv().asScala
    env
      .get("ACTIONS_STEP_DEBUG") // GitHub Actions will set this env variable if "Re-Run with Debug Logging" is selected
      .orElse(env.get("DEBUG")) // This is just a general-purpose environment variable
      .exists(_.toBoolean)
  }

  def make[F[_]: Async](
    containerLogsDirectory: Option[Path] = Some(Path("byzantine-tests") / "target" / "logs"),
    debugLoggingEnabled:    Boolean = loggingEnabledFromEnvironment
  ): Resource[F, (DockerSupport[F], DockerClient)] =
    for {
      implicit0(dockerClient: DockerClient) <- Resource.make(Sync[F].blocking(DefaultDockerClient.fromEnv().build()))(
        c => Sync[F].blocking(c.close())
      )
      nodeCache <- Resource.make[F, Ref[F, Set[BifrostDockerTetraNode]]](Ref.of(Set.empty[BifrostDockerTetraNode]))(
        _.get.flatMap(
          _.toList
            .traverse(node =>
              Sync[F]
                .blocking(dockerClient.removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill))
                .voidError
            )
            .void
        )
      )
      networkCache <- Resource.make[F, Ref[F, Set[NetworkCreation]]](Ref.of(Set.empty[NetworkCreation]))(
        _.get.flatMap(
          _.toList
            .traverse(network => Sync[F].blocking(dockerClient.removeNetwork(network.id())).voidError)
            .void
        )
      )
      _ <- containerLogsDirectory.fold(Resource.unit[F])(Files[F].createDirectories(_).toResource)
      dockerSupport = new Impl[F](containerLogsDirectory, debugLoggingEnabled, nodeCache, networkCache)
    } yield (dockerSupport, dockerClient)

  private class Impl[F[_]: Async](
    containerLogsDirectory: Option[Path],
    debugLoggingEnabled:    Boolean,
    nodeCache:              Ref[F, Set[BifrostDockerTetraNode]],
    networkCache:           Ref[F, Set[NetworkCreation]]
  )(implicit dockerClient: DockerClient)
      extends DockerSupport[F] {

    def createNode(name: String, nodeGroupName: String, config: TestNodeConfig): Resource[F, BifrostDockerTetraNode] =
      for {
        node <- Resource.make(createContainer(name, nodeGroupName, config))(node =>
          Sync[F].defer(node.stop[F]) >>
          containerLogsDirectory
            .map(_ / s"$name-${node.containerId.take(8)}.log")
            .fold(Applicative[F].unit)(node.saveContainerLogs[F]) >>
          deleteContainer(node)
        )
        _ <- Resource.onFinalize(Sync[F].defer(nodeCache.update(_ - node)))
        _ <- node.configure(config.yaml).toResource
      } yield node

    private def createContainer(
      name:          String,
      nodeGroupName: String,
      config:        TestNodeConfig
    ): F[BifrostDockerTetraNode] = {
      val networkNamePrefix: String = "bifrost-it"
      for {
        networkName <- (networkNamePrefix + nodeGroupName).pure[F]
        environment = Map("BIFROST_LOG_LEVEL" -> (if (debugLoggingEnabled) "DEBUG" else "INFO"))
        containerConfig   <- buildContainerConfig(name, environment, config).pure[F]
        containerCreation <- Sync[F].blocking(dockerClient.createContainer(containerConfig, name))
        node              <- BifrostDockerTetraNode(containerCreation.id(), name, config).pure[F]
        _                 <- nodeCache.update(_ + node)
        networkId <- Sync[F].blocking(dockerClient.listNetworks().asScala.find(_.name == networkName)).flatMap {
          case Some(network) => network.id().pure[F]
          case None =>
            Sync[F]
              .blocking(dockerClient.createNetwork(NetworkConfig.builder().name(networkName).build()))
              .flatTap(n => networkCache.update(_ + n))
              .map(_.id())
        }
        _ <- Sync[F].blocking(dockerClient.connectToNetwork(node.containerId, networkId))
      } yield node
    }

    private def deleteContainer(node: BifrostDockerTetraNode): F[Unit] =
      Sync[F].blocking(
        dockerClient.removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill)
      )

    private def buildContainerConfig(
      name:        String,
      environment: Map[String, String],
      config:      TestNodeConfig
    ): ContainerConfig = {
      val configDirectory = "/opt/docker/config"
      val bifrostImage: String = s"toplprotocol/bifrost-node:${BuildInfo.version}"
      val exposedPorts: Seq[String] = List(config.rpcPort, config.p2pPort, config.jmxRemotePort).map(_.toString)
      val env =
        environment.toList.map { case (key, value) => s"$key=$value" }

      val cmd =
        List(
          s"-Dcom.sun.management.jmxremote.port=${config.jmxRemotePort}",
          s"-Dcom.sun.management.jmxremote.rmi.port=${config.jmxRemotePort}",
          "-Dcom.sun.management.jmxremote.ssl=false",
          "-Dcom.sun.management.jmxremote.local.only=false",
          "-Dcom.sun.management.jmxremote.authenticate=false",
          "--config",
          "/opt/docker/config/node.yaml",
          "--logbackFile",
          "/opt/docker/config/logback.xml",
          "--debug"
        )

      val hostConfig =
        HostConfig.builder().build()

      ContainerConfig
        .builder()
        .image(bifrostImage)
        .env(env: _*)
        .volumes(configDirectory)
        .cmd(cmd: _*)
        .hostname(name)
        .hostConfig(hostConfig)
        .exposedPorts(exposedPorts: _*)
        .build()
    }
  }
}

case class TestNodeConfig(
  bigBangTimestamp: Instant = Instant.now().plusSeconds(5),
  stakerCount:      Int = 1,
  localStakerIndex: Int = 0,
  knownPeers:       List[String] = Nil,
  stakes:           Option[List[BigInt]] = None,
  rpcPort:          Int = 9084,
  p2pPort:          Int = 9085,
  jmxRemotePort:    Int = 9083,
  genusEnabled:     Boolean = false,
  genusRpcPort:     Int = 9091
) {

  def yaml: String = {
    val stakesStr = stakes.fold("")(
      _.map(v => s"\"$v\"").mkString("stakes: [", ",", "]")
    )
    s"""
       |bifrost:
       |  rpc:
       |    bind-host: 0.0.0.0
       |    port: "$rpcPort"
       |  p2p:
       |    bind-host: 0.0.0.0
       |    port: "$p2pPort"
       |    known-peers: "${knownPeers.map(p => s"$p:9085").mkString(",")}"
       |  big-bang:
       |    staker-count: $stakerCount
       |    local-staker-index: $localStakerIndex
       |    timestamp: ${bigBangTimestamp.toEpochMilli}
       |    $stakesStr
       |  protocols:
       |    0:
       |      slot-duration: 200 milli
       |genus:
       |  enable: "$genusEnabled"
       |  rpc:
       |    port: "$genusRpcPort"
       |    node:
       |      host: "localhost"
       |      port: "$rpcPort"
       |      tls: "false"
       |""".stripMargin
  }

}
