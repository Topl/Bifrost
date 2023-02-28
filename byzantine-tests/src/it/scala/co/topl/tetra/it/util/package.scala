package co.topl.tetra.it

import co.topl.algebras.ToplRpc
import com.spotify.docker.client.DockerClient

import scala.language.implicitConversions

package object util {

  implicit def rpcToRpcApi[F[_]](rpc: ToplRpc[F, fs2.Stream[F, *]]): NodeRpcApi[F] =
    new NodeRpcApi(rpc)

  implicit def nodeToDockerApi(node: BifrostDockerTetraNode)(implicit
    dockerClient:                    DockerClient
  ): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}
