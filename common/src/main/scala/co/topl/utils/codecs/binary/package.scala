package co.topl.utils.codecs

package object binary {

  trait Codecs extends valuetypes.Codecs with attestation.Codecs with modifier.Codecs

  object codecs extends Codecs
}
