package co.topl.utils.codecs

package object binary {

  trait Codecs extends valuetypes.Codecs with attestation.Codecs

  trait Implicits extends valuetypes.Implicits with attestation.Implicits

  object codecs extends Codecs

  object implicits extends Implicits
}
