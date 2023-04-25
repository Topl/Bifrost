package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses._
import co.topl.models.UnsignedBlockHeader

trait TetraSignableCodecs {

  implicit val signableUnsignedConsensusBlockHeader: Signable[UnsignedBlockHeader] =
    Signable.fromScodecEncoder(TetraScodecCodecs.unsignedBlockHeaderCodec)

}

object TetraSignableCodecs extends TetraSignableCodecs
