package co.topl.codecs.binary.show

import co.topl.attestation.AddressCodec.implicits._
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.attestation._
import co.topl.codecs.binary.scodecs._
import co.topl.codecs.binary.typeclasses.BinaryShow
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.hash.digest.{Digest, Digest32}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.BloomFilter
import co.topl.modifier.box.{AssetCode, Box, BoxId, ProgramId, SecurityRoot}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.IdiomaticScalaTransition.implicits.toAttemptOps
import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.implicits._
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}
import com.google.common.primitives.Longs
import scodec.bits.{BitVector, ByteVector}

trait BinaryShowInstances {
  implicit val bytesBinaryShow: BinaryShow[Array[Byte]] = x => x

  implicit val bitsBinaryShow: BinaryShow[BitVector] = x => x.toByteArray

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

  implicit val evidenceBinaryShow: BinaryShow[Evidence] = _.evBytes

  implicit val signatureCurve25519BinaryShow: BinaryShow[SignatureCurve25519] =
    BinaryShow.fromEncoder(signatureCurve25519Codec.asEncoder)

  implicit val thresholdSignatureCurve25519BinaryShow: BinaryShow[ThresholdSignatureCurve25519] =
    BinaryShow.fromEncoder(thresholdSignatureCurve25519Codec.asEncoder)

  implicit val signatureEd25519BinaryShow: BinaryShow[SignatureEd25519] =
    BinaryShow.fromEncoder(signatureEd25519Codec.asEncoder)

  implicit val proofBinaryShow: BinaryShow[Proof[_ <: Proposition]] =
    BinaryShow.fromEncoder(proofCodec.asEncoder)

  implicit val publicKeyPropositionCurve25519BinaryShow: BinaryShow[PublicKeyPropositionCurve25519] =
    BinaryShow.fromEncoder(publicKeyPropositionCurve25519Codec.asEncoder)

  implicit val publicKeyPropositionEd25519BinaryShow: BinaryShow[PublicKeyPropositionEd25519] =
    BinaryShow.fromEncoder(publicKeyPropositionEd25519Codec.asEncoder)

  implicit val thresholdPropositionCurve25519BinaryShow: BinaryShow[ThresholdPropositionCurve25519] =
    BinaryShow.fromEncoder(thresholdPropositionCurve25519Codec.asEncoder)

  implicit val privateKeyCurve25519BinaryShow: BinaryShow[PrivateKeyCurve25519] =
    BinaryShow.fromEncoder

  implicit val privateKeyEd25519BinaryShow: BinaryShow[PrivateKeyEd25519] =
    BinaryShow.fromEncoder

  implicit val securityRootBinaryShow: BinaryShow[SecurityRoot] = _.root

  implicit val boxIdBinaryShow: BinaryShow[BoxId] = digestBinaryShow[Digest32].map(_.hash)

  implicit val assetCodeBinaryShow: BinaryShow[AssetCode] = BinaryShow.fromEncoder

  implicit val bloomFilterBinaryShow: BinaryShow[BloomFilter] = bloomFilter =>
    bloomFilter.value.flatMap(Longs.toByteArray)

  implicit val txBinaryShow: BinaryShow[Transaction.TX] = BinaryShow.fromEncoder

  implicit val modifierIdBinaryShow: BinaryShow[ModifierId] = _.value

  implicit val boxBinaryShow: BinaryShow[Box[_]] = BinaryShow.fromEncoder

  implicit val programIdBinaryShow: BinaryShow[ProgramId] = _.hashBytes
}
