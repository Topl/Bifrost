package co.topl.nodeCodecs.binary.scodecs.network

import co.topl.nodeCodecs.binary.scodecs.network.message.MessageCodecs
import co.topl.nodeCodecs.binary.scodecs.network.peer.PeerCodecs

trait NetworkCodecs extends MessageCodecs with PeerCodecs
