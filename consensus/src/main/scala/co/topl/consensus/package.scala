package co.topl

import co.topl.models.{Bytes, TypedBytes}

package object consensus {
  type Proof = Bytes
  type Hash = Bytes
  type SecretKey = TypedBytes
  type PublicKey = TypedBytes
}
