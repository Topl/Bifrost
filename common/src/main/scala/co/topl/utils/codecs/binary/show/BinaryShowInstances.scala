package co.topl.utils.codecs.binary.show

import co.topl.attestation.{Address, Proposition}
import co.topl.attestation.AddressCodec.implicits._
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toAttemptOps
import co.topl.utils.SizedBytes
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}
import co.topl.utils.codecs.binary.scodecs._
import co.topl.utils.codecs.binary.typeclasses.BinaryShow
import co.topl.utils.SizedBytes.implicits._
import scodec.bits.ByteVector

trait BinaryShowInstances {
  implicit val bytesBinaryShow: BinaryShow[Array[Byte]] = x => x

  implicit val base58BinaryShow: BinaryShow[Base58Data] = _.value

  implicit val base16BinaryShow: BinaryShow[Base16Data] = _.value

  implicit def digestBinaryShow[T: Digest]: BinaryShow[T] = _.bytes

  implicit val addressBinaryShow: BinaryShow[Address] = { address =>
    (for {
      addressBitVector <- addressCodec.encode(address)
      addressBytes = addressBitVector.toByteArray
    } yield addressBytes ++ addressBytes.checksum)
      .getOrThrow()
  }

  implicit def sizedBytesBinaryShow[T: SizedBytes]: BinaryShow[T] = _.toArray

  implicit val byteVectorBinaryShow: BinaryShow[ByteVector] = _.toArray

  implicit val propositionBinaryShow: BinaryShow[Proposition] = BinaryShow.fromEncoder

  implicit val publicKeyBinaryShow: BinaryShow[PublicKey] = _.value
}
