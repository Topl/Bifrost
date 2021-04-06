package co.topl.it

import akka.actor.ActorSystem
import co.topl.akkahttprpc.RequestModifier
import com.spotify.docker.client.DockerClient

package object util {

  implicit def nodeToRpcApi(
    node:            BifrostDockerNode
  )(implicit system: ActorSystem, dockerClient: DockerClient): NodeRpcApi =
    NodeRpcApi(node)

  implicit def nodeToDockerApi(node: BifrostDockerNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    NodeDockerApi(node)

  implicit def nodeToRpcRequestModifier(implicit
    node:         BifrostDockerNode,
    system:       ActorSystem,
    dockerClient: DockerClient
  ): RequestModifier =
    node.rpcRequestModifier
}
