package co.topl.utils.codecs

package object json {
  trait Codecs extends crypto.Codecs with valuetypes.Codecs

  object codecs extends Codecs
}
