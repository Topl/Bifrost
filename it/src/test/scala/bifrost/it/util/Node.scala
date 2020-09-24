package bifrost.it.util

import bifrost.it.api.NodeApi
import bifrost.settings.AppSettings

import scala.concurrent.ExecutionContext

case class Node(settings: AppSettings, containerId: String, networkPort: Int, rpcPort: Int)
               (implicit override val ec: ExecutionContext) extends NodeApi {

  override val restAddress: String = "localhost"
  override val nodeRpcPort: Int = rpcPort
}
