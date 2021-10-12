package co.topl.utils.codecs.binary.crypto

import co.topl.crypto.PublicKey
import co.topl.utils.codecs.binary.valuetypes.codecs._
import scodec.Codec

object PublicKeyCodec {

  def codec(size: Int): Codec[PublicKey] =
    bytes(size).xmap[PublicKey](bytes => PublicKey(bytes), pk => pk.value).as[PublicKey]

  trait Codecs {
    def publicKey(size: Int): Codec[PublicKey] = codec(size)
  }

  object codecs extends Codecs
}
