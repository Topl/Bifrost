package co.topl.tetra.it

import com.spotify.docker.client.DockerClient

import scala.language.implicitConversions

package object util {

  implicit def nodeToRpcApi(
    node: BifrostDockerTetraNode
  )(implicit dockerClient: DockerClient): NodeRpcApi =
    NodeRpcApi(node)

  implicit def nodeToDockerApi(node: BifrostDockerTetraNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    NodeDockerApi(node)
}
