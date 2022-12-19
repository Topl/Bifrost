package co.topl.typeclasses

import co.topl.crypto.signing.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.proto.models.{PropositionKnowledgeEd25519, VerificationKeyEd25519}
import com.google.protobuf.ByteString

/**
 * Indicates that some value T can produce a Verification Key
 */
@simulacrum.typeclass
trait ContainsVerificationKey[T] {

  /**
   * Constructs a verification key using the given value T
   */
  @simulacrum.op("vk")
  def verificationKeyOf(t: T): VerificationKey
}

object ContainsVerificationKey {

  trait Instances {

    // TODO in PR BN-714 v3, we should stop depending typeclasses on models, and move to proto Models
    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.Ed25519] = {
      key =>
        val propositionKey = PropositionKnowledgeEd25519.of(
          Some(
            VerificationKeyEd25519.of(
              ByteString.copyFrom(key.bytes.data.toArray)
            )
          )
        )
        val verificationKeyEd2551 = new Ed25519().getVerificationKey(propositionKey)
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(verificationKeyEd2551.value.toByteArray))(bytesLength, Lengths.`32`))
    }
  }

  object instances extends Instances
}
