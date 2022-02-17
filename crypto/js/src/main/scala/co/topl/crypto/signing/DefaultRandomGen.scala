package co.topl.crypto.signing

import java.util.Random

trait DefaultRandomGen {

  def defaultRandom: Random = defaultRandom(None)

  def defaultRandom(seed: Option[Seed]): Random = {
    val random = new Random()
    // TODO: seed.foreach(_.value).foreach(random.setSeed)

    random.nextBytes(
      Array(0: Byte)
    ) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random
  }
}
