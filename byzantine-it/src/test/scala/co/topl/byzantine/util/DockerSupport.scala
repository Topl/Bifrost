package co.topl.byzantine.util

import cats.Applicative
import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import co.topl.buildinfo.node.BuildInfo
import co.topl.consensus.models.StakingAddress
import co.topl.typeclasses.implicits._
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
  ): Resource[F, BifrostDockerNode]
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
    containerLogsDirectory: Option[Path] = Some(Path("byzantine-it") / "target" / "logs"),
    debugLoggingEnabled:    Boolean = loggingEnabledFromEnvironment
  ): Resource[F, (DockerSupport[F], DockerClient)] =
    for {
      implicit0(dockerClient: DockerClient) <- Resource.make(Sync[F].blocking(DefaultDockerClient.fromEnv().build()))(
        c => Sync[F].blocking(c.close())
      )
      nodeCache <- Resource.make[F, Ref[F, Set[BifrostDockerNode]]](Ref.of(Set.empty[BifrostDockerNode]))(
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
      _ <- containerLogsDirectory.fold(Resource.unit[F])(Files.forAsync[F].createDirectories(_).toResource)
      dockerSupport = new Impl[F](containerLogsDirectory, debugLoggingEnabled, nodeCache, networkCache)
    } yield (dockerSupport, dockerClient)

  private class Impl[F[_]: Async](
    containerLogsDirectory: Option[Path],
    debugLoggingEnabled:    Boolean,
    nodeCache:              Ref[F, Set[BifrostDockerNode]],
    networkCache:           Ref[F, Set[NetworkCreation]]
  )(implicit dockerClient: DockerClient)
      extends DockerSupport[F] {

    def createNode(name: String, nodeGroupName: String, config: TestNodeConfig): Resource[F, BifrostDockerNode] =
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
    ): F[BifrostDockerNode] = {
      val networkNamePrefix: String = "bifrost-it"
      for {
        networkName <- (networkNamePrefix + nodeGroupName).pure[F]
        environment = Map("BIFROST_LOG_LEVEL" -> (if (debugLoggingEnabled) "DEBUG" else "INFO"))
        containerConfig   <- buildContainerConfig(name, environment, config).pure[F]
        containerCreation <- Sync[F].blocking(dockerClient.createContainer(containerConfig, name))
        node              <- BifrostDockerNode(containerCreation.id(), name, config).pure[F]
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

    private def deleteContainer(node: BifrostDockerNode): F[Unit] =
      Sync[F].blocking(
        dockerClient.removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill)
      )

    private def buildContainerConfig(
      name:        String,
      environment: Map[String, String],
      config:      TestNodeConfig
    ): ContainerConfig = {
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
          "--logbackFile",
          "/bifrost/config/logback.xml",
          "--debug"
        )

      val hostConfig =
        config.stakingBindSourceDir
          .foldLeft(HostConfig.builder().privileged(true))((b, sourceDir) =>
            b.appendBinds(
              HostConfig.Bind
                .builder()
                .from(sourceDir)
                .to("/bifrost-staking")
                .selinuxLabeling(true)
                .build()
            )
          )
          .build()

      ContainerConfig
        .builder()
        .image(bifrostImage)
        .env(env: _*)
        .cmd(cmd: _*)
        .hostname(name)
        .hostConfig(hostConfig)
        .exposedPorts(exposedPorts: _*)
        .build()
    }
  }
}

case class TestNodeConfig(
  bigBangTimestamp:     Instant = Instant.now().plusSeconds(5),
  stakerCount:          Int = 1,
  localStakerIndex:     Int = 0,
  knownPeers:           List[String] = Nil,
  stakes:               Option[List[BigInt]] = None,
  rpcPort:              Int = 9084,
  p2pPort:              Int = 9085,
  jmxRemotePort:        Int = 9083,
  genusEnabled:         Boolean = false,
  stakingBindSourceDir: Option[String] = None,
  serverHost:           Option[String] = None,
  serverPort:           Option[Int] = None,
  stakingAddress:       Option[StakingAddress] = None
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
       |  staking:
       |    staking-address: ${stakingAddress.fold("")(_.show)}
       |  p2p:
       |    bind-host: 0.0.0.0
       |    ${serverHost.map(sh => s"public-host: $sh").getOrElse("")}
       |    port: "$p2pPort"
       |    ${serverPort.map(sp => s"public-port: $sp").getOrElse("")}
       |    known-peers: "${knownPeers.map(p => s"$p:9085").mkString(",")}"
       |  big-bang:
       |    staker-count: $stakerCount
       |    local-staker-index: $localStakerIndex
       |    timestamp: ${bigBangTimestamp.toEpochMilli}
       |    $stakesStr
       |  protocols:
       |    0:
       |      slot-duration: 500 milli
       |      chain-selection-k-lookback: 6
       |      operational-periods-per-epoch: 2
       |genus:
       |  enable: "$genusEnabled"
       |""".stripMargin
  }

}

object TestNodeConfig {
  val epochSlotLength: Long = 150 // See co.topl.node.ApplicationConfig.Bifrost.Protocol
}
