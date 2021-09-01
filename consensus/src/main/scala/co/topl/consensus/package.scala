package co.topl

import co.topl.models.{Bytes, TypedBytes}

package object consensus {
  type Hash = Bytes
  type SecretKey = TypedBytes
  type PublicKey = TypedBytes
}
