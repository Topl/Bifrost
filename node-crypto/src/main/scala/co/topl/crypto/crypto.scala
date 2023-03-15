package co.topl

import java.security.SecureRandom

package object crypto {

  def defaultRandom: SecureRandom = defaultRandom(None)

  def defaultRandom(seed: Option[Array[Byte]]): SecureRandom = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    seed.foreach(random.setSeed)

    random.nextBytes(
      Array(0: Byte)
    ) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random
  }
}
