package co.topl.crypto

import io.estatico.newtype.macros.newtype

import java.security.SecureRandom
import scala.language.implicitConversions

package object signing {

  @newtype
  case class MessageToSign(value: Array[Byte])

  @newtype
  case class Seed(value: Array[Byte])

  // default method for generating randomness to seed a signature scheme
  def defaultRandom: Array[Byte] = {
    val random = SecureRandom.getInstance("SHA1PRNG", "SUN")
    random.nextBytes(
      Array(0: Byte)
    ) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random.generateSeed(128)
  }
}
