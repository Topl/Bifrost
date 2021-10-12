package co.topl.utils.codecs.binary.attestation

package object proposition {

  trait Implicits
      extends PropositionCodec.Implicits
      with PublicKeyPropositionEd25519Codec.Implicits
      with PublicKeyPropositionCurve25519Codec.Implicits
      with ThresholdPropositionCurve25519Codec.Implicits

  trait Codecs
      extends PropositionCodec.Codecs
      with PublicKeyPropositionEd25519Codec.Codecs
      with PublicKeyPropositionCurve25519Codec.Codecs
      with ThresholdPropositionCurve25519Codec.Codecs

  object implicits extends Implicits
  object codecs extends Codecs
}
