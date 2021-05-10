package co.topl.crypto

import cats.Eq
import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object signatures {

  @newtype
  case class PrivateKey(value: Array[Byte])

  object PrivateKey {

    trait Instances {
      implicit val eqPrivateKey: Eq[PrivateKey] = _.value sameElements _.value
    }
  }

  @newtype
  case class PublicKey(value: Array[Byte])

  object PublicKey {}

  @newtype
  case class SharedSecret(value: Array[Byte])

  @newtype
  case class Signature(value: Array[Byte])

  object implicits extends PrivateKey.Instances

  type MessageToSign = Array[Byte]

}
