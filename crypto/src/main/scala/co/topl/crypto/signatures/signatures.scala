package co.topl.crypto

import cats.Eq
import co.topl.crypto.hash.HashFailure
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

  @newtype
  case class SharedSecret(value: Array[Byte])

  @newtype
  case class Signature(value: Array[Byte])

  type MessageToSign = Array[Byte]

  type CreateKeyPairResult = Either[CreateKeyPairFailure, (PrivateKey, PublicKey)]

  sealed trait CreateKeyPairFailure
  case class PrivateKeyHashFailure(failure: HashFailure) extends CreateKeyPairFailure

  trait CreateKeyPairResultOps {
    def instance: CreateKeyPairResult

    /**
     * Gets the valid hash result or throws an exception.
     *
     * @param orThrow an override for the exception to throw
     * @return a valid hash result
     */
    def getOrThrow(
      orThrow: CreateKeyPairFailure => Throwable = e => new Exception(e.toString)
    ): (PrivateKey, PublicKey) =
      instance match {
        case Right(a) => a
        case Left(e)  => throw orThrow(e)
      }
  }

  trait ToCreateKeyPairResultOps {

    implicit def toCreateKeyPairResultOps(result: CreateKeyPairResult): CreateKeyPairResultOps =
      new CreateKeyPairResultOps {
        def instance: CreateKeyPairResult = result
      }
  }

  object implicits extends PrivateKey.Instances with ToCreateKeyPairResultOps
}
