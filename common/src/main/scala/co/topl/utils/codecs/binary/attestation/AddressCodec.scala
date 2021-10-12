package co.topl.utils.codecs.binary.attestation

import co.topl.attestation.Address
import co.topl.utils.codecs.binary.codecs.byte
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import co.topl.utils.codecs.binary.attestation.EvidenceCodec.codecs._

object AddressCodec {

  def encode(value: Address): Attempt[BitVector] =
    for {
      prefixBits   <- byte.encode(value.networkPrefix)
      evidenceBits <- evidence.encode(value.evidence)
    } yield prefixBits ++ evidenceBits

  def decode(from: BitVector): Attempt[DecodeResult[Address]] =
    for {
      prefixDecodeResult <- byte.decode(from)
      prefixValue = prefixDecodeResult.value
      prefixDecodeRemaining = prefixDecodeResult.remainder
      evidenceDecodeResult <- evidence.decode(prefixDecodeRemaining)
      evidenceValue = evidenceDecodeResult.value
      evidenceDecodeRemaining = evidenceDecodeResult.remainder
    } yield DecodeResult(Address(evidenceValue)(prefixValue), evidenceDecodeRemaining)

  val codec: Codec[Address] = new Codec[Address] {
    override def encode(value: Address): Attempt[BitVector] = AddressCodec.encode(value)

    override def sizeBound: SizeBound = byte.sizeBound + evidence.sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[Address]] = AddressCodec.decode(bits)
  }

  trait Codecs {
    val address: Codec[Address] = codec
  }

  trait Implicits {
    implicit val implicitAddress: Codec[Address] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}
