package co.topl.consensus

import co.topl.settings.AppSettings

package object consensus {
  def setProtocolMngr(settings: AppSettings): Unit = {
    protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)
  }
}
