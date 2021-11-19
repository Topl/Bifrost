package co.topl.nodeCodecs.binary.scodecs

import co.topl.nodeCodecs.binary.scodecs.network.NetworkCodecs
import co.topl.nodeCodecs.binary.scodecs.settings.SettingsCodecs

trait ScodecCodecs extends NetworkCodecs with SettingsCodecs
