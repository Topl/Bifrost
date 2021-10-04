package co.topl.crypto

import io.estatico.newtype.macros.newtype

package object signatures {

  @newtype
  case class Signature(value: Array[Byte])
}
