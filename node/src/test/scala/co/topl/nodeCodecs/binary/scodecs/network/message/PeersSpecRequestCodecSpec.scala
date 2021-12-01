package co.topl.nodeCodecs.binary.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.message.Messages.MessagesV1.PeersSpecRequest
import co.topl.network.message.Transmission
import co.topl.nodeCodecs.binary.legacy.network.message.GetPeersSerializer
import org.scalacheck.Gen
import co.topl.nodeCatsInstances._
import co.topl.utils.StringDataTypes.Base16Data
import scodec.bits.BitVector

class PeersSpecRequestCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peers spec request",
    peersSpecRequestCodec,
    new GetPeersSerializer(),
    Gen.const(PeersSpecRequest())
  )
}
