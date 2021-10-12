package co.topl.utils.codecs.binary.attestation

package object proof {

  trait Codecs
      extends ProofCodec.Codecs
      with SignatureCurve25519Codec.Codecs
      with SignatureEd25519Codec.Codecs
      with ThresholdSignatureCurve25519Codec.Codecs

  trait Implicits
      extends ProofCodec.Implicits
      with SignatureCurve25519Codec.Implicits
      with SignatureEd25519Codec.Implicits
      with ThresholdSignatureCurve25519Codec.Implicits

  object codecs extends Codecs
  object implicits extends Implicits
}
