package co.topl

import co.topl.models.{Bytes, TypedBytes}

import scala.language.implicitConversions

package object consensus {
  type Hash = Bytes
  type SecretKey = TypedBytes
  type PublicKey = TypedBytes
}
