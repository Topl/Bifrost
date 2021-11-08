package co.topl.crypto

import io.estatico.newtype.macros.newtype

import java.security.SecureRandom
import scala.language.implicitConversions

package object signing {

  @newtype
  case class MessageToSign(value: Array[Byte])

  @newtype
  case class Seed(value: Array[Byte])

  def defaultRandom: SecureRandom = defaultRandom(None)

  def defaultRandom(seed: Option[Seed]): SecureRandom = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    seed.map(_.value).foreach(random.setSeed)

    random.nextBytes(
      Array(0: Byte)
    ) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random
  }

  /**
   * The type of the password used alongside a mnemonic to generate a SecretKey
   */
  type Password = String
}
