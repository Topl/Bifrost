package co.topl.utils

import co.topl.consensus.ProtocolVersioner

trait ProtocolVersionHelper {

  implicit val protocolVersioner: ProtocolVersioner = ProtocolVersioner.default

}
