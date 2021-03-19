
package co.topl.it.util

import akka.actor.ActorSystem
import co.topl.it.api.NodeApi
import co.topl.settings.AppSettings
import com.spotify.docker.client.DockerClient

import scala.concurrent.ExecutionContext

case class Node(settings: AppSettings, containerId: String, networkPort: Int, rpcPort: Int)
               (implicit override val ec: ExecutionContext, val system: ActorSystem, client: DockerClient) extends NodeApi {

  override val restAddress: String =
    client.inspectContainer(containerId).networkSettings().ipAddress()

  override val nodeRpcPort: Int = rpcPort
}