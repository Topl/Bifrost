package co.topl.crypto.generation

import cats.implicits._
import co.topl.crypto.generation.InitializationFailures.InvalidSizeByteLength
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, Language}
import co.topl.crypto.signing._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import scodec.bits.BitVector
import simulacrum.typeclass

import java.util.UUID

@typeclass trait KeyInitializer[SK] {
  self =>

  /**
   * Creates a random secret key
   */
  def random(): SK

  /**
   * Creates a secret key from the given seed
   */
  def fromEntropy(entropy: Entropy, password: Option[Password] = None): SK

  /**
   * Creates an instance of a secret key given a byte vector
   * @param bytes bytes of the secret key
   * @return
   */
  def fromBytes(bytes: Bytes): Either[InitializationFailure, SK]

  /**
   * @param mnemonicString
   * @param language
   * @param password
   * @return
   */
  def fromMnemonicString(
    mnemonicString: String
  )(language:       Language = Language.English, password: Option[Password] = None): Either[InitializationFailure, SK] =
    Entropy
      .fromMnemonicString(mnemonicString, language)
      .map(fromEntropy(_, password))
      .leftMap(e => InitializationFailures.FailedToCreateEntropy(e))

  def fromBase58String(base58String: String): Either[InitializationFailure, SK] = for {
    bits <- Either.fromOption(BitVector.fromBase58(base58String), InitializationFailures.InvalidBase58String)
    sk   <- fromBytes(bits.toByteVector)
  } yield sk

  def fromBase16String(base16String: String): Either[InitializationFailure, SK] = for {
    bits <- Either.fromOption(BitVector.fromHex(base16String), InitializationFailures.InvalidBase16String)
    sk   <- fromBytes(bits.toByteVector)
  } yield sk
}

object KeyInitializer {

  trait Instances {

    implicit def curve25519Initializer(implicit curve25519: Curve25519): KeyInitializer[SecretKeys.Curve25519] =
      new KeyInitializer[SecretKeys.Curve25519] {

        override def random(): SecretKeys.Curve25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.Curve25519 =
          curve25519.deriveKeyPairFromEntropy(entropy, password)._1

        override def fromBytes(bytes: Bytes): Either[InvalidSizeByteLength, SecretKeys.Curve25519] =
          Sized
            .strict[Bytes, SecretKeys.Curve25519.Length](bytes)
            .map(SecretKeys.Curve25519(_))
            .leftMap(InitializationFailures.InvalidSizeByteLength)
      }

    implicit def ed25519Initializer(implicit ed25519: Ed25519): KeyInitializer[SecretKeys.Ed25519] =
      new KeyInitializer[SecretKeys.Ed25519] {

        override def random(): SecretKeys.Ed25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.Ed25519 =
          ed25519.deriveKeyPairFromEntropy(entropy, password)._1

        override def fromBytes(bytes: Bytes): Either[InvalidSizeByteLength, SecretKeys.Ed25519] =
          Sized
            .strict[Bytes, SecretKeys.Ed25519.Length](bytes)
            .map(SecretKeys.Ed25519(_))
            .leftMap(InitializationFailures.InvalidSizeByteLength)

      }

    implicit def vrfInitializer(implicit ed25519VRF: Ed25519VRF): KeyInitializer[SecretKeys.VrfEd25519] =
      new KeyInitializer[SecretKeys.VrfEd25519] {

        def random(): SecretKeys.VrfEd25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.VrfEd25519 =
          ed25519VRF.deriveKeyPairFromEntropy(entropy, password)._1

        override def fromBytes(bytes: Bytes): Either[InvalidSizeByteLength, SecretKeys.VrfEd25519] =
          Sized
            .strict[Bytes, SecretKeys.VrfEd25519.Length](bytes)
            .map(SecretKeys.VrfEd25519(_))
            .leftMap(InitializationFailures.InvalidSizeByteLength)
      }

    implicit def extendedEd25519Initializer(implicit
      extendedEd25519: ExtendedEd25519
    ): KeyInitializer[SecretKeys.ExtendedEd25519] =
      new KeyInitializer[SecretKeys.ExtendedEd25519] {

        def random(): SecretKeys.ExtendedEd25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.ExtendedEd25519 =
          extendedEd25519.deriveKeyPairFromEntropy(entropy, password)._1

        def fromBytes(bytes: Bytes): Either[InitializationFailure, SecretKeys.ExtendedEd25519] = for {
          _ <- Either.cond(bytes.length == 96, bytes, InitializationFailures.InvalidByteLength)
          left = bytes.slice(0, 32)
          right = bytes.slice(32, 64)
          chainCode = bytes.slice(64, 96)
        } yield SecretKeys.ExtendedEd25519(
          Sized.strictUnsafe(left),
          Sized.strictUnsafe(right),
          Sized.strictUnsafe(chainCode)
        )

      }
  }

  object Instances extends Instances
}

sealed abstract class InitializationFailure

object InitializationFailures {
  case class KeyCreationError(throwable: Throwable) extends InitializationFailure
  case object InvalidBase58String extends InitializationFailure
  case object InvalidBase16String extends InitializationFailure
  case object InvalidByteLength extends InitializationFailure
  case class InvalidSizeByteLength(sizedByteError: Sized.InvalidLength) extends InitializationFailure
  case class FailedToCreateEntropy(entropyFailure: EntropyFailure) extends InitializationFailure
}
