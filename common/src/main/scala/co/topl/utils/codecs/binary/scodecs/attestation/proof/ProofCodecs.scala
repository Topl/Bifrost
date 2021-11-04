package co.topl.utils.codecs.binary.scodecs.attestation.proof

import co.topl.attestation.{
  Proof,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  SignatureCurve25519,
  SignatureEd25519,
  ThresholdPropositionCurve25519,
  ThresholdSignatureCurve25519
}
import co.topl.utils.codecs.binary.scodecs.crypto.signatureCodec
import co.topl.utils.codecs.binary.scodecs.valuetypes.{byteCodec, listCodec}
import scodec.{Attempt, Codec, Err}
import scodec.codecs.discriminated

trait ProofCodecs {

  implicit val signatureCurve25519Codec: Codec[SignatureCurve25519] =
    signatureCodec(SignatureCurve25519.signatureSize).as[SignatureCurve25519]

  implicit val signatureEd25519Codec: Codec[SignatureEd25519] =
    signatureCodec(SignatureEd25519.signatureSize).as[SignatureEd25519]

  implicit val thresholdSignatureCurve25519Codec: Codec[ThresholdSignatureCurve25519] =
    listCodec[SignatureCurve25519]
      .xmap[Set[SignatureCurve25519]](_.toSet, _.toList)
      .as[ThresholdSignatureCurve25519]

  implicit val proofCodec: Codec[Proof[_]] =
    discriminated[Proof[_]]
      .by(byteCodec)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, signatureCurve25519Codec)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdSignatureCurve25519Codec)
      .typecase(PublicKeyPropositionEd25519.typePrefix, signatureEd25519Codec)

  implicit val proofPropositionCurve25519Codec: Codec[Proof[PublicKeyPropositionCurve25519]] =
    signatureCurve25519Codec
      .exmap[Proof[PublicKeyPropositionCurve25519]](
        signature => Attempt.successful(signature),
        {
          case x: SignatureCurve25519 => Attempt.successful(x)
          case _                      => Attempt.failure(Err("proof is not of type SignatureCurve25519"))
        }
      )
      .as[Proof[PublicKeyPropositionCurve25519]]

  implicit val proofPropositionEd25519Codec: Codec[Proof[PublicKeyPropositionEd25519]] =
    signatureEd25519Codec
      .exmap[Proof[PublicKeyPropositionEd25519]](
        signature => Attempt.successful(signature),
        {
          case x: SignatureEd25519 => Attempt.successful(x)
          case _                   => Attempt.failure(Err("proof is not of type SignatureEd25519"))
        }
      )
      .as[Proof[PublicKeyPropositionEd25519]]

  implicit val proofPropositionThresholdCurve25519Codec: Codec[Proof[ThresholdPropositionCurve25519]] =
    thresholdSignatureCurve25519Codec
      .exmap[Proof[ThresholdPropositionCurve25519]](
        signature => Attempt.successful(signature),
        {
          case x: ThresholdSignatureCurve25519 => Attempt.successful(x)
          case _ => Attempt.failure(Err("proof is not of type ThresholdSignatureCurve25519"))
        }
      )
      .as[Proof[ThresholdPropositionCurve25519]]
}
