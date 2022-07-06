package co.topl

import cats.Eq
import co.topl.crypto.hash.digest
import co.topl.crypto.signing.Seed
import io.estatico.newtype.macros.newtype

import java.security.SecureRandom
import scala.language.implicitConversions

package object crypto {

  def defaultRandom: SecureRandom = defaultRandom(None)

  def defaultRandom(seed: Option[Seed]): SecureRandom = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    seed.map(_.value).foreach(random.setSeed)

    random.nextBytes(
      Array(0: Byte)
    ) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random
  }

  // todo: deprecatee
  @newtype
  case class Signature(value: Array[Byte])

  @newtype
  case class PrivateKey(value: Array[Byte])

  object PrivateKey {

    trait Instances {
      implicit val eqPrivateKey: Eq[PrivateKey] = _.value sameElements _.value
    }
  }

  @newtype
  case class PublicKey(value: Array[Byte])

  trait Implicits
      extends digest.Instances
      with digest.Digest.ToDigestOps
      with hash.Instances
      with PrivateKey.Instances
      with catsinstances.Implicits

  object implicits extends Implicits
}
