package bifrost.network

import bifrost.settings.NetworkSettings
import bifrost.utils.Logging
import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}

import scala.collection.JavaConverters._

package object upnp extends Logging {

  def getValidGateway(settings: NetworkSettings): Option[UPnPGateway] = {
    try {
      log.info("Looking for UPnP gateway device...")
      val defaultHttpReadTimeout = settings.upnpGatewayTimeout.map(_.toMillis.toInt).getOrElse(GatewayDevice.getHttpReadTimeout)
      GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout)
      val discover = new GatewayDiscover()
      val defaultDiscoverTimeout = settings.upnpDiscoverTimeout.map(_.toMillis.toInt).getOrElse(discover.getTimeout)
      discover.setTimeout(defaultDiscoverTimeout)

      val gatewayMap = Option(discover.discover).map(_.asScala).map(_.toMap).getOrElse(Map())
      if (gatewayMap.isEmpty) {
        log.debug("There are no UPnP gateway devices")
        None
      } else {
        gatewayMap.foreach { case (addr, _) =>
          log.debug("UPnP gateway device found on " + addr.getHostAddress)
        }
        val gateway = Option(discover.getValidGateway)
        if (gateway.isEmpty) {
          log.debug("There is no connected UPnP gateway device")
        }
        gateway.map(new UPnPGateway(_))
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to discover UPnP gateway devices", t)
        None
    }
  }
}
