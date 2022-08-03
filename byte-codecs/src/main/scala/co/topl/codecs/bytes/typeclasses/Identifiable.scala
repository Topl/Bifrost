package co.topl.codecs.bytes.typeclasses

import scodec.bits.ByteVector
import simulacrum.{op, typeclass}

/**
 * Satisfies that T can be uniquely identified
 */
@typeclass trait Identifiable[T] {
  @op("id") def idOf(t: T): (Byte, ByteVector)
}
