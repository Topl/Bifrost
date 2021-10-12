package co.topl.utils.codecs.binary

package object attestation {

  trait Implicits
      extends proof.Implicits
      with proposition.Implicits
      with EvidenceCodec.Implicits
      with AddressCodec.Implicits

  trait Codecs extends proof.Codecs with proposition.Codecs with EvidenceCodec.Codecs with AddressCodec.Codecs

  object codecs extends Codecs
  object implicits extends Implicits
}
