package co.topl.network.codecs.scodecs

import co.topl.network.codecs.scodecs.network.NetworkCodecs
import co.topl.network.codecs.scodecs.settings.SettingsCodecs

trait ScodecCodecs extends NetworkCodecs with SettingsCodecs
