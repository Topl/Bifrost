package co.topl.crypto.kes.signatures

import co.topl.crypto.PublicKey

trait ProductSignature

case class AsymmetricSignature(sigi: Array[Byte], sigm: Array[Byte], pki: PublicKey, offset: Long, pkl: PublicKey)
    extends ProductSignature

case class SymmetricSignature(sigi: Array[Byte], sigm: Array[Byte], pki: PublicKey, offset: Long, pkl: PublicKey)
    extends ProductSignature
