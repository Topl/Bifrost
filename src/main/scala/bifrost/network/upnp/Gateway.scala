package bifrost.network.upnp

import java.net.{InetAddress, InetSocketAddress}

import bifrost.utils.Logging
import org.bitlet.weupnp.{GatewayDevice, PortMappingEntry}

class Gateway(gateway: GatewayDevice) extends Logging {

  val localAddress: InetAddress = gateway.getLocalAddress
  val externalAddress: InetAddress = InetAddress.getByName(gateway.getExternalIPAddress)

  log.debug("Using UPnP gateway device on " + localAddress.getHostAddress)
  log.info("External IP address is " + externalAddress.getHostAddress)


  def addPort(port: Int): Unit = {
    try {
      if (gateway.addPortMapping(port, port, localAddress.getHostAddress, "TCP", "BifrostClient")) {
        log.debug("Mapped port [" + externalAddress.getHostAddress + "]:" + port)
      } else {
        log.debug("Unable to map port " + port)
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to map port " + port + ": " + t.toString)
    }
  }

  def deletePort(port: Int): Unit = {
    try {
      if (gateway.deletePortMapping(port, "TCP")) {
        log.debug("Mapping deleted for port " + port)
      } else {
        log.debug("Unable to delete mapping for port " + port)
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to delete mapping for port " + port + ": " + t.toString)
    }
  }

  def getLocalAddressForExternalPort(externalPort: Int):Option[InetSocketAddress] = {
    try {
      val entry = new PortMappingEntry
      if (gateway.getSpecificPortMappingEntry(externalPort, "TCP", entry)) {
        val host = entry.getInternalClient
        Some(new InetSocketAddress(InetAddress.getByName(host), entry.getInternalPort))
      } else {
        None
      }
    } catch {
      case t: Throwable =>
        log.error("Unable to get local address for external port " + externalPort + ": " + t.toString)
        None
    }
  }
}
