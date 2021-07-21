package co.topl.primitives

import java.security.SecureRandom
import org.bouncycastle.math.ec.rfc7748.X25519

/**
  * AMS 2020:
  * Key exchange using X25519
  * Implements RFC7748 Section 6.1 Diffie-Hellman key exchange with base point 9
  */

class Ecx {

  private val basePoint:Array[Byte] = {
    val out = Array.fill(X25519.POINT_SIZE){0x00.toByte}
    out.update(0,0x09.toByte)
    out
  }

  def generateSK:Array[Byte] = {
    val a:Array[Byte] = Array.fill(X25519.POINT_SIZE){0x00.toByte}
    X25519.generatePrivateKey(new SecureRandom,a:Array[Byte])
    a
  }

  def scalarMultBasePoint(a:Array[Byte]):Array[Byte] = {
    val ka:Array[Byte] = Array.fill(X25519.POINT_SIZE){0x00.toByte}
    X25519.scalarMult(a,0,basePoint,0,ka,0)
    ka
  }

  def scalarMult(a:Array[Byte],kb:Array[Byte]):Array[Byte] = {
    val kab:Array[Byte] = Array.fill(X25519.POINT_SIZE){0x00.toByte}
    X25519.scalarMult(a,0,kb,0,kab,0)
    kab
  }

}
