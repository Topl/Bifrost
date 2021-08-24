package co.topl.typeclasses.crypto

import co.topl.models.Bytes

trait Signer[PrivateKey, PublicKey] {
  def sign(): Bytes
}
