package co.topl.codecs.bytes.tetra

import cats.{Eq, Show}
import co.topl.codecs.bytes.CodecSpec
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{KesBinaryTree, Ratio}
import co.topl.models._
import org.scalacheck.Gen

class TetraScodecCodecsSpec extends CodecSpec {

  import ModelGenerators._
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

  codecBehavior[Latin1Data](
    "Latin1Data",
    TetraScodecCodecs.latin1DataCodec,
    ModelGenerators.latin1DataGen
  )

  codecBehavior[Int128](
    "Int128",
    TetraScodecCodecs.int128Codec,
    ModelGenerators.arbitraryInt128.arbitrary
  )

  codecBehavior[KesBinaryTree](
    "KesBinaryTree",
    TetraScodecCodecs.kesBinaryTreeCodec,
    ModelGenerators.kesBinaryTreeGen
  )

  codecBehavior[VerificationKeys.Curve25519](
    "VerificationKeys.Curve25519",
    TetraScodecCodecs.vkCurve25519Codec,
    ModelGenerators.arbitraryCurve25519VK.arbitrary
  )

  codecBehavior[VerificationKeys.Ed25519](
    "VerificationKeys.Ed25519",
    TetraScodecCodecs.vkEd25519Codec,
    ModelGenerators.ed25519VkGen
  )

  codecBehavior[VerificationKeys.ExtendedEd25519](
    "VerificationKeys.ExtendedEd25519",
    TetraScodecCodecs.vkExtendedEd25519Codec,
    ModelGenerators.extendedEd25519VkGen
  )

  codecBehavior[VerificationKeys.VrfEd25519](
    "VerificationKeys.VrfEd25519",
    TetraScodecCodecs.vkVrfCodec,
    ModelGenerators.vkVrfEd25519Gen
  )

  codecBehavior[VerificationKeys.KesSum](
    "VerificationKeys.KesSum",
    TetraScodecCodecs.vkKesSumCodec,
    ModelGenerators.vkKesSumGen
  )

  codecBehavior[VerificationKeys.KesProduct](
    "VerificationKeys.KesProduct",
    TetraScodecCodecs.vkKesProductCodec,
    ModelGenerators.kesVKGen
  )

  codecBehavior[Proofs.Knowledge.Curve25519](
    "Proofs.Knowledge.Curve25519",
    TetraScodecCodecs.proofSignatureCurve25519,
    ModelGenerators.curve25519ProofGen
  )

  codecBehavior[Proofs.Knowledge.Ed25519](
    "Proofs.Knowledge.Ed25519",
    TetraScodecCodecs.proofSignatureEd25519Codec,
    ModelGenerators.ed25519ProofGen
  )

  codecBehavior[Proofs.Knowledge.VrfEd25519](
    "Proofs.Knowledge.VrfEd25519",
    TetraScodecCodecs.proofSignatureVrfCodec,
    ModelGenerators.proofVrfEd25519Gen
  )

  codecBehavior[Proofs.Knowledge.KesSum](
    "Proofs.Knowledge.KesSum",
    TetraScodecCodecs.proofSignatureKesSumCodec,
    ModelGenerators.kesSumProofGen
  )

  codecBehavior[Proofs.Knowledge.KesProduct](
    "Proofs.Knowledge.KesProduct",
    TetraScodecCodecs.proofSignatureKesProductCodec,
    ModelGenerators.kesProductProofGen
  )

  codecBehavior[SecretKeys.Curve25519](
    "SecretKeys.Curve25519",
    TetraScodecCodecs.secretKeyCurve25519Codec,
    ModelGenerators.arbitraryCurve25519SK.arbitrary
  )

  codecBehavior[SecretKeys.Ed25519](
    "SecretKeys.Ed25519",
    TetraScodecCodecs.secretKeyEd25519Codec,
    ModelGenerators.arbitraryEdSK.arbitrary
  )

  codecBehavior[SecretKeys.ExtendedEd25519](
    "SecretKeys.ExtendedEd25519",
    TetraScodecCodecs.secretKeyExtendedEd25519Codec,
    ModelGenerators.arbitraryExtendedEdSK.arbitrary
  )

  codecBehavior[SecretKeys.VrfEd25519](
    "SecretKeys.VrfEd25519",
    TetraScodecCodecs.secretKeyVrfCodec,
    ModelGenerators.skVrfEd25519Gen
  )

  codecBehavior[SecretKeys.KesSum](
    "SecretKeys.KesSum",
    TetraScodecCodecs.secretKeyKesSumCodec,
    ModelGenerators.kesSumSKGen
  )

  codecBehavior[SecretKeys.KesProduct](
    "SecretKeys.KesProduct",
    TetraScodecCodecs.secretKeyKesProductCodec,
    ModelGenerators.kesProductSKGen
  )

  codecBehavior[EligibilityCertificate](
    "EligibilityCertificate",
    TetraScodecCodecs.eligibilityCertificateCodec,
    ModelGenerators.eligibilityCertificateGen
  )

  codecBehavior[BlockHeader.Unsigned.PartialOperationalCertificate](
    "BlockHeader.Unsigned.PartialOperationalCertificate",
    TetraScodecCodecs.partialOperationalCertificateCodec,
    ModelGenerators.partialOperationalCertificateGen
  )

  codecBehavior[StakingAddress]()

  codecBehavior[BlockHeader.Unsigned](
    "BlockHeader.Unsigned",
    TetraScodecCodecs.unsignedBlockHeaderCodec,
    ModelGenerators.unsignedHeaderGen()
  )

  codecBehavior[BlockHeader]()

  codecBehavior[TypedEvidence](
    "TypedEvidence",
    TetraScodecCodecs.typedEvidenceCodec,
    ModelGenerators.typedEvidenceGen
  )

  codecBehavior[SpendingAddress]()

  codecBehavior[Proposition]()

  codecBehavior[Proof]()

  codecBehavior[Box]()

  codecBehavior[Transaction]()

  codecBehavior[Transaction.Unproven]()
}
