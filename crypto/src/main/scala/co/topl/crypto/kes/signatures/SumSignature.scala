package co.topl.crypto.kes.signatures

import co.topl.crypto.kes.keys.PublicKey

case class SumSignature(bytes: Array[Byte], offset: Long, pkl: PublicKey)
