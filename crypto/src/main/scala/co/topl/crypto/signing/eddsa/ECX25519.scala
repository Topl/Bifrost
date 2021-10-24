package co.topl.crypto.signing.eddsa

import java.security.SecureRandom

/**
 * AMS 2021:
 * Key exchange using X25519
 * Implements RFC7748 Section 6.1 Diffie-Hellman key exchange with base point 9
 */

class ECX25519 {

  private val x25519: X25519 = new X25519

  private val basePoint: Array[Byte] = {
    val out = Array.fill(x25519.POINT_SIZE)(0x00.toByte)
    out.update(0, 0x09.toByte)
    out
  }

  private[signing] def generateSK: Array[Byte] = {
    val a: Array[Byte] = Array.fill(x25519.POINT_SIZE)(0x00.toByte)
    x25519.generatePrivateKey(new SecureRandom, a: Array[Byte])
    a
  }

  private[signing] def scalarMultBasePoint(a: Array[Byte]): Array[Byte] = {
    val ka: Array[Byte] = Array.fill(x25519.POINT_SIZE)(0x00.toByte)
    x25519.scalarMult(a, 0, basePoint, 0, ka, 0)
    ka
  }

  private[signing] def scalarMult(a: Array[Byte], kb: Array[Byte]): Array[Byte] = {
    val kab: Array[Byte] = Array.fill(x25519.POINT_SIZE)(0x00.toByte)
    x25519.scalarMult(a, 0, kb, 0, kab, 0)
    kab
  }

}
