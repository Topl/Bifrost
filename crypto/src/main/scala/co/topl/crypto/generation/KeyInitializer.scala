package co.topl.crypto.generation

import cats.implicits._
import co.topl.crypto.generation.InitializationFailures.InvalidSizeByteLength
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, Language}
import co.topl.crypto.signing._
import co.topl.proto.models.{PropositionKnowledgeEd25519, VerificationKeyEd25519}
import co.topl.protobuf._
import co.topl.protobuf.utility.HasLength.instances._
import co.topl.protobuf.utility.Sized
import com.google.protobuf.ByteString
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

    implicit def ed25519Initializer(implicit ed25519: Ed25519): KeyInitializer[PropositionKnowledgeEd25519] =
      new KeyInitializer[PropositionKnowledgeEd25519] {

        override def random(): PropositionKnowledgeEd25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[Password]): PropositionKnowledgeEd25519 =
          ed25519.deriveKeyPairFromEntropy(entropy, password)._1

        override def fromBytes(bytes: Bytes): Either[InvalidSizeByteLength, PropositionKnowledgeEd25519] =
          Sized
            .strict[Bytes, Ed25519Sized.Length](bytes)
            .map(sized =>
              PropositionKnowledgeEd25519.of(
                Some(
                  VerificationKeyEd25519.of(
                    ByteString.copyFrom(sized.data.toArray)
                  )
                )
              )
            )
            .leftMap(InitializationFailures.InvalidSizeByteLength)

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
