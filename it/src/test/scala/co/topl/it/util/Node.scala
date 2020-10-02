package co.topl.it.util

import bifrost.it.api.NodeApi
import bifrost.settings.AppSettings
import co.topl.it.api.NodeApi
import co.topl.settings.AppSettings

import scala.concurrent.ExecutionContext

case class Node(settings: AppSettings, containerId: String, networkPort: Int, rpcPort: Int)
               (implicit override val ec: ExecutionContext) extends NodeApi {

  override val restAddress: String = "localhost"
  override val nodeRpcPort: Int = rpcPort
}
