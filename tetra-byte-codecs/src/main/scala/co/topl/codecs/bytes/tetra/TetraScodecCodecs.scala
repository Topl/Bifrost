package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.scodecs._
import co.topl.consensus.models._
import co.topl.crypto.{models => nodeCryptoModels}
import co.topl.models._
import co.topl.models.utility._
import scodec.codecs.discriminated
import scodec.codecs.lazily
import scodec.Codec
import shapeless.::
import shapeless.HList
import shapeless.HNil

/**
 * Use this object or the package object to access all of the codecs from outside of this package.
 */
object TetraScodecCodecs extends TetraScodecCodecs

trait TetraScodecCodecs {

  // todo: JAA - consider implications of variable vs. fixed length (BigInt vs. Int128)
  // (I think this is never transmitted so probably safe if we used built in BigInt)

  implicit val bigIntCodec: Codec[BigInt] =
    byteArrayCodec.xmapc(t => BigInt(t))(t => t.toByteArray)

  implicit val ratioCodec: Codec[Ratio] =
    (bigIntCodec :: bigIntCodec)
      .xmapc { case numerator :: denominator :: HNil =>
        Ratio(numerator, denominator)
      } { ratio =>
        HList(
          ratio.numerator,
          ratio.denominator
        )
      }
      .as[Ratio]

  implicit val unknownFieldSetCodec: Codec[scalapb.UnknownFieldSet] =
    emptyCodec(scalapb.UnknownFieldSet.empty)

  implicit val nodeCryptoKesBinaryTreeCodec: Codec[nodeCryptoModels.KesBinaryTree] = {
    val kesBinaryTreeEmptyCodec: Codec[nodeCryptoModels.KesBinaryTree.Empty] =
      emptyCodec(nodeCryptoModels.KesBinaryTree.Empty())

    val nodeCryptoKesBinaryTreeLeafCodec: Codec[nodeCryptoModels.KesBinaryTree.SigningLeaf] =
      (byteArrayCodecSized(32) :: byteArrayCodecSized(32))
        .as[nodeCryptoModels.KesBinaryTree.SigningLeaf]

    val nodeCryptoKesBinaryTreeNodeCodec: Codec[nodeCryptoModels.KesBinaryTree.MerkleNode] =
      lazily(
        (byteArrayCodecSized(32) :: byteArrayCodecSized(32) :: byteArrayCodecSized(
          32
        ) :: nodeCryptoKesBinaryTreeCodec :: nodeCryptoKesBinaryTreeCodec)
          .as[nodeCryptoModels.KesBinaryTree.MerkleNode]
      )

    discriminated[nodeCryptoModels.KesBinaryTree]
      .by(byteCodec)
      .typecase(nodeCryptoModels.KesBinaryTree.emptyTypePrefix, kesBinaryTreeEmptyCodec)
      .typecase(nodeCryptoModels.KesBinaryTree.leafTypePrefix, nodeCryptoKesBinaryTreeLeafCodec)
      .typecase(nodeCryptoModels.KesBinaryTree.nodeTypePrefix, nodeCryptoKesBinaryTreeNodeCodec)
  }

  implicit val nodeCryptoSignatureKesSumCodec: Codec[nodeCryptoModels.SignatureKesSum] =
    (byteArrayCodecSized(32) :: byteArrayCodecSized(64) :: seqCodec(byteArrayCodecSized(32)))
      .as[nodeCryptoModels.SignatureKesSum]

  implicit val nodeCryptoSecretKeyKesSumCodec: Codec[nodeCryptoModels.SecretKeyKesSum] =
    (nodeCryptoKesBinaryTreeCodec :: uLongCodec)
      .as[nodeCryptoModels.SecretKeyKesSum]

  implicit val nodeCryptoSecretKeyKesProductCodec: Codec[nodeCryptoModels.SecretKeyKesProduct] =
    (
      nodeCryptoKesBinaryTreeCodec ::
        nodeCryptoKesBinaryTreeCodec ::
        byteArrayCodecSized(32) ::
        nodeCryptoSignatureKesSumCodec ::
        longCodec
    )
      .as[nodeCryptoModels.SecretKeyKesProduct]

  implicit val blockIdCodec: Codec[BlockId] =
    byteStringCodecSized(32).xmap(BlockId(_), _.value)

  implicit val consensusEligibilityCertificateCodec: Codec[EligibilityCertificate] =
    (byteStringCodecSized(80) :: // vrfSig
      byteStringCodecSized(32) :: // vrfVk
      byteStringCodecSized(32) :: // thresholdEvidence
      byteStringCodecSized(32) :: // eta
      emptyCodec(scalapb.UnknownFieldSet.empty))
      .as[EligibilityCertificate]

  implicit val vkKesProductCodec: Codec[VerificationKeyKesProduct] =
    (byteStringCodecSized(32) :: intCodec :: unknownFieldSetCodec)
      .as[VerificationKeyKesProduct]

  implicit val signatureKesSumCodec: Codec[SignatureKesSum] =
    (byteStringCodecSized(32) :: byteStringCodecSized(64) :: seqCodec(byteStringCodecSized(32)) :: unknownFieldSetCodec)
      .as[SignatureKesSum]

  implicit val signatureKesProductCodec: Codec[SignatureKesProduct] =
    (signatureKesSumCodec :: signatureKesSumCodec :: byteStringCodecSized(32) :: unknownFieldSetCodec)
      .as[SignatureKesProduct]

  implicit val operationalCertificateCodec: Codec[OperationalCertificate] =
    (
      vkKesProductCodec ::
        signatureKesProductCodec ::
        byteStringCodecSized(32) ::
        byteStringCodecSized(64) ::
        unknownFieldSetCodec
    )
      .as[OperationalCertificate]

  implicit val partialOperationalCertificateCodec: Codec[UnsignedBlockHeader.PartialOperationalCertificate] =
    (vkKesProductCodec :: signatureKesProductCodec :: byteStringCodecSized(32))
      .as[UnsignedBlockHeader.PartialOperationalCertificate]

  implicit val stakingAddressCodec: Codec[StakingAddress] =
    (byteStringCodecSized(32) :: unknownFieldSetCodec).as[StakingAddress]

  implicit val versionCodec: Codec[ProtocolVersion] =
    (intCodec :: intCodec :: intCodec :: unknownFieldSetCodec).as[ProtocolVersion]

  implicit val consensusBlockHeaderCodec: Codec[BlockHeader] = (
    emptyCodec[Option[BlockId]](None) :: // headerId
      blockIdCodec :: // parentHeaderId
      longCodec :: // parentSlot
      byteStringCodecSized(32) :: // txRoot
      byteStringCodecSized(256) :: // bloomFilter
      longCodec :: // timestamp
      longCodec :: // height
      longCodec :: // slot
      consensusEligibilityCertificateCodec ::
      operationalCertificateCodec ::
      byteStringCodec :: // metadata
      stakingAddressCodec :: // address
      versionCodec ::
      unknownFieldSetCodec
  ).as[BlockHeader]

  implicit val unsignedBlockHeaderCodec: Codec[UnsignedBlockHeader] = (
    blockIdCodec :: // parentHeaderId
      longCodec :: // parentSlot
      byteStringCodecSized(32) :: // txRoot
      byteStringCodecSized(256) :: // bloomFilter
      longCodec :: // timestamp
      longCodec :: // height
      longCodec :: // slot
      consensusEligibilityCertificateCodec ::
      partialOperationalCertificateCodec ::
      byteStringCodec :: // metadata
      stakingAddressCodec // address
  ).as[UnsignedBlockHeader]
}
