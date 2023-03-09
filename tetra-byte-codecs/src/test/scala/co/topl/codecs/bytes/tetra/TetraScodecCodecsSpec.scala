package co.topl.codecs.bytes.tetra

import cats.{Eq, Show}
import co.topl.codecs.bytes.CodecSpec
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.EligibilityCertificate
import co.topl.crypto.models.KesBinaryTree
import co.topl.models.utility.Ratio
import co.topl.models._
import org.scalacheck.Gen

class TetraScodecCodecsSpec extends CodecSpec {

  import TetraScodecCodecs._

  implicit def defaultShow[T]: Show[T] = Show.fromToString
  implicit def defaultEq[T]: Eq[T] = Eq.fromUniversalEquals

  codecBehavior[BigInt](
    "BigInt",
    TetraScodecCodecs.bigIntCodec,
    Gen.long.map(BigInt(_))
  )

  codecBehavior[Ratio](
    "Ratio",
    TetraScodecCodecs.ratioCodec,
    ModelGenerators.ratioGen
  )

  codecBehavior[KesBinaryTree](
    "KesBinaryTree",
    TetraScodecCodecs.nodeCryptoKesBinaryTreeCodec,
    co.topl.crypto.utils.NodeCryptoGenerators.kesBinaryTreeGen
  )

  codecBehavior[co.topl.crypto.models.SignatureKesSum](
    "co.topl.crypto.models.SignatureKesSum",
    nodeCryptoSignatureKesSumCodec,
    co.topl.crypto.utils.NodeCryptoGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[co.topl.crypto.models.SecretKeyKesSum](
    "co.topl.crypto.models.SecretKeyKesSum",
    nodeCryptoSecretKeyKesSumCodec,
    co.topl.crypto.utils.NodeCryptoGenerators.kesSumSKGen
  )

  codecBehavior[co.topl.crypto.models.SecretKeyKesProduct](
    "co.topl.crypto.models.SecretKeyKesProduct",
    nodeCryptoSecretKeyKesProductCodec,
    co.topl.crypto.utils.NodeCryptoGenerators.kesProductSKGen
  )

  codecBehavior[BlockId](
    "co.topl.consensus.models.BlockId",
    TetraScodecCodecs.blockIdCodec,
    co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary
  )

  codecBehavior[EligibilityCertificate](
    "co.topl.consensus.models.EligibilityCertificate",
    TetraScodecCodecs.consensusEligibilityCertificateCodec,
    co.topl.models.generators.consensus.ModelGenerators.arbitraryEligibilityCertificate.arbitrary
  )

  codecBehavior[co.topl.consensus.models.VerificationKeyKesProduct](
    "co.topl.consensus.models.VerificationKeyKesProduct",
    TetraScodecCodecs.vkKesProductCodec,
    co.topl.models.generators.consensus.ModelGenerators.arbitraryVerificationKeyKesProduct.arbitrary
  )

  codecBehavior[co.topl.consensus.models.SignatureKesSum](
    "co.topl.consensus.models.SignatureKesSum",
    TetraScodecCodecs.signatureKesSumCodec,
    co.topl.models.generators.consensus.ModelGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[co.topl.consensus.models.SignatureKesProduct](
    "co.topl.consensus.models.SignatureKesProduct",
    TetraScodecCodecs.signatureKesProductCodec,
    co.topl.models.generators.consensus.ModelGenerators.signatureKesProductArbitrary.arbitrary
  )

  codecBehavior[co.topl.consensus.models.OperationalCertificate](
    "co.topl.consensus.models.OperationalCertificate",
    TetraScodecCodecs.operationalCertificateCodec,
    co.topl.models.generators.consensus.ModelGenerators.arbitraryOperationalCertificate.arbitrary
  )

  codecBehavior[co.topl.models.UnsignedBlockHeader.PartialOperationalCertificate](
    "co.topl.models.UnsignedBlockHeader.PartialOperationalCertificate",
    TetraScodecCodecs.partialOperationalCertificateCodec,
    ModelGenerators.partialOperationalCertificateGen
  )

  codecBehavior[co.topl.consensus.models.BlockHeader](
    "co.topl.consensus.models.BlockHeader",
    TetraScodecCodecs.consensusBlockHeaderCodec,
    co.topl.models.generators.consensus.ModelGenerators.headerGen()
  )

  codecBehavior[co.topl.models.UnsignedBlockHeader](
    "co.topl.models.UnsignedBlockHeader",
    TetraScodecCodecs.unsignedBlockHeaderCodec,
    ModelGenerators.unsignedHeaderGen()
  )

}
