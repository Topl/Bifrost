package co.topl.network.peer

import co.topl.settings.Version

import java.net.InetSocketAddress

/**
 * Declared information about peer
 *
 * @param agentName Network agent name. May contain information about client code
 *                  stack, starting from core code-base up to the end graphical interface.
 *                  Basic format is `/Name:Version(comments)/Name:Version/.../`,
 *                  e.g. `/Ergo-Scala-client:2.0.0(iPad; U; CPU OS 3_2_1)/AndroidBuild:0.8/`
 * @param version Identifies protocol version being used by the node
 * @param nodeName Custom node name
 * @param declaredAddress Public network address of the node if any
 * @param features Set of node capabilities
 */
case class PeerMetadata(
  agentName:       String,
  version:         Version,
  nodeName:        String,
  declaredAddress: Option[InetSocketAddress],
  features:        Seq[PeerFeature]
) {

  lazy val localAddressOpt: Option[InetSocketAddress] =
    features.collectFirst { case LocalAddressPeerFeature(addr) => addr }

  def reachablePeer: Boolean = address.isDefined

  def address: Option[InetSocketAddress] = declaredAddress orElse localAddressOpt
}
