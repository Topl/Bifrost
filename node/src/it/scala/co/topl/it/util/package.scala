package co.topl.it

import akka.actor.ActorSystem
import com.spotify.docker.client.DockerClient

import scala.language.implicitConversions

package object util {

  implicit def nodeToRpcApi(
    node:            BifrostDockerNode
  )(implicit system: ActorSystem, dockerClient: DockerClient): NodeRpcApi =
    NodeRpcApi(node)

  implicit def nodeToDockerApi(node: BifrostDockerNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    NodeDockerApi(node)
}
