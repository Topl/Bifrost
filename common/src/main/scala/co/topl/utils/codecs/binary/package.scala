package co.topl.utils.codecs

package object binary {

  trait Codecs extends valuetypes.Codecs with attestation.Codecs with modifier.Codecs with crypto.Codecs

  trait Implicits extends valuetypes.Implicits

  object codecs extends Codecs
  object implicits extends Implicits
}
