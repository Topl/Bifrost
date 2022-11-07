package co.topl.codecs.binary.typeclasses

import co.topl.codecs.bytes.tetra.TetraSignableCodecs
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import scodec.{Attempt, Encoder}

import scala.util.Try

/**
 * Defines instances of the [[Signable]] typeclass.
 */
object SignableInstances {

  /**
   * Instances of [[Codec]] and [[Encoder]] typeclasses which encode values into their "signable" bits.
   *
   * Intended for usage in defining [[Signable]] instances.
   */
  object SignableCodecs {

    /**
     * Encoder for generating a signable-message from a [[Transaction.Unproven]].
     * @param blake2b256 instance of [[Blake2b256]]
     * @return an encoder to generate the signable-message of a [[Transaction.Unproven]]
     */
    def tetraTransactionEncoder(implicit blake2b256: Blake2b256): Encoder[Transaction.Unproven] =
      Encoder(transaction =>
        Attempt.fromTry(
          Try(
            TetraSignableCodecs.signableUnprovenTransaction.signableBytes(transaction).toBitVector
          )
        )
      )
  }

  /**
   * Instances of the [[Signable]] typeclass for generating messages that can be used to create proofs for transactions
   * and blocks.
   */
  trait Instances {

    /**
     * [[Signable]] instance for a [[Transaction.Unproven]].
     *
     * @param blake2b256 an instance of [[Blake2b256]]
     * @return a [[Signable]] instance for generating signable-messages from a [[Transaction.Unproven]] value
     */
    implicit def unprovenTransactionSignable(implicit blake2b256: Blake2b256): Signable[Transaction.Unproven] =
      Signable.fromScodecEncoder(SignableCodecs.tetraTransactionEncoder)
  }

  trait Implicits extends Instances

  object implicits extends Implicits

}
