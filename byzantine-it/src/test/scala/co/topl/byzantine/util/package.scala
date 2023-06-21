package co.topl.byzantine

import co.topl.algebras.ToplGenusRpc
import com.spotify.docker.client.DockerClient
import scala.language.implicitConversions

package object util {

  implicit def rpcToGenusRpcApi[F[_]](rpc: ToplGenusRpc[F]): GenusRpcApi[F] =
    new GenusRpcApi(rpc)

  implicit def nodeToDockerApi(node: BifrostDockerNode)(implicit
                                                        dockerClient: DockerClient
  ): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}
