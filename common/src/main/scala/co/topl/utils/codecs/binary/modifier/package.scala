package co.topl.utils.codecs.binary

package object modifier {
  trait Codecs extends box.Codecs with transaction.Codecs

  object codecs extends Codecs
}
