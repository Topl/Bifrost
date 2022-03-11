package co.topl.network.codecs.scodecs.network

import co.topl.network.codecs.scodecs.network.message.MessageCodecs
import co.topl.network.codecs.scodecs.network.peer.PeerCodecs

trait NetworkCodecs extends MessageCodecs with PeerCodecs
