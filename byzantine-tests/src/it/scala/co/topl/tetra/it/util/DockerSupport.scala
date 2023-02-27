package co.topl.tetra.it.util

import cats.effect._
import cats.implicits._
import co.topl.buildinfo.node.BuildInfo
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, NetworkConfig, NetworkCreation}
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}

import java.time.Instant
import scala.jdk.CollectionConverters._

trait DockerSupport[F[_]] {

  def createNode(
    name:          String,
    nodeGroupName: String,
    configYaml:    String = ""
  ): Resource[F, BifrostDockerTetraNode]

}

object DockerSupport {

  def make[F[_]: Async]: Resource[F, (DockerSupport[F], DockerClient)] =
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
      dockerSupport = new DockerSupport[F] {

        def createNode(name: String, nodeGroupName: String, configYaml: String): Resource[F, BifrostDockerTetraNode] =
          Resource
            .make(createContainer(name, nodeGroupName))(node =>
              Sync[F]
                .blocking(dockerClient.removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill))
            )
            .evalTap(_.configure(configYaml))

        private def createContainer(name: String, nodeGroupName: String): F[BifrostDockerTetraNode] =
          for {
            networkName       <- (DockerSupport.networkNamePrefix + nodeGroupName).pure[F]
            containerConfig   <- buildContainerConfig(name, Map.empty).pure[F]
            containerCreation <- Sync[F].blocking(dockerClient.createContainer(containerConfig, name))
            node              <- BifrostDockerTetraNode(containerCreation.id(), name).pure[F]
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
              "/opt/docker/config/node.yaml",
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
      }
    } yield (dockerSupport, dockerClient)

  val configDirectory = "/opt/docker/config"

  val bifrostImage: String = s"toplprotocol/bifrost-node:${BuildInfo.version}"
  val networkNamePrefix: String = "bifrost-it"

  val rpcPortKeyPath = "bifrost.rpc.bind-port"
  val rpcPort = "9084"
  val p2pPort = "9085"
  val jmxRemotePort = "9083"
  val exposedPorts: Seq[String] = List(rpcPort, p2pPort, jmxRemotePort)
}

object DefaultConfig {

  def apply(
    bigBangTimestamp: Instant = Instant.now().plusSeconds(5),
    stakerCount:      Int = 1,
    localStakerIndex: Int = 0,
    knownPeers:       List[String] = Nil
  ): String =
    s"""
       |bifrost:
       |  rpc:
       |    bind-host: 0.0.0.0
       |    port: 9084
       |  p2p:
       |    bind-host: 0.0.0.0
       |    port: 9085
       |    known-peers: "${knownPeers.map(p => s"$p:9085").mkString(",")}"
       |  big-bang:
       |    staker-count: $stakerCount
       |    local-staker-index: $localStakerIndex
       |    timestamp: ${bigBangTimestamp.toEpochMilli}
       |""".stripMargin
}
