package co.topl.tetra.it

import co.topl.algebras.{NodeRpc, ToplGenusRpc}
import com.spotify.docker.client.DockerClient
import scala.language.implicitConversions

package object util {

  implicit def rpcToRpcApi[F[_]](rpc: NodeRpc[F, fs2.Stream[F, *]]): NodeRpcApi[F] =
    new NodeRpcApi(rpc)

  implicit def rpcToGenusRpcApi[F[_]](rpc: ToplGenusRpc[F]): GenusRpcApi[F] =
    new GenusRpcApi(rpc)

  implicit def nodeToDockerApi(node: BifrostDockerTetraNode)(implicit
    dockerClient: DockerClient
  ): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}
