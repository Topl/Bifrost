package co.topl.utils.codecs.binary.crypto

import co.topl.crypto.signatures.Signature
import scodec.Codec
import co.topl.utils.codecs.binary.valuetypes.codecs._

object SignatureCodec {

  def codec(size: Int): Codec[Signature] =
    bytes(size).xmap[Signature](bytes => Signature(bytes), signature => signature.value).as[Signature]

  trait Codecs {
    def signature(size: Int): Codec[Signature] = codec(size)
  }

  object codecs extends Codecs
}
