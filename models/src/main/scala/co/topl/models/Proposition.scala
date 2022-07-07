package co.topl.models

import scala.collection.immutable.ListSet
import scala.language.implicitConversions

/**
 * Encodes the "spending logic" of a Box.  Roughly translates to a function `(Proof, Blockchain Context) => Boolean`
 */
sealed trait Proposition

object Propositions {

  /**
   * A Proposition that can never be satisfied, thus rendering the box unlockable
   */
  case object PermanentlyLocked extends Proposition

  object Knowledge {

    /**
     * Requires "proof of knowledge of the secret key corresponding to the proposition's VK" to unlock the box
     */
    case class Curve25519(key: VerificationKeys.Curve25519) extends Proposition

    /**
     * Requires "proof of knowledge of the secret key corresponding to the proposition's VK" to unlock the box
     */
    case class Ed25519(key: VerificationKeys.Ed25519) extends Proposition

    /**
     * Requires "proof of knowledge of the secret key corresponding to the proposition's VK" to unlock the box
     */
    case class ExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition

    /**
     * Requires "proof of knowledge of bytes which hash to the proposition's digest" to unlock the box
     */
    case class HashLock(valueDigest: Digest32) extends Proposition
  }

  object Compositional {

    /**
     * A proposition which includes sub-propositions and a number indicating the
     * lower bound of valid proofs that must be provided
     */
    case class Threshold(threshold: Int, propositions: ListSet[Proposition]) extends Proposition

    /**
     * A proposition requiring that both sub-propositions be satisfied
     */
    case class And(a: Proposition, b: Proposition) extends Proposition

    /**
     * A proposition requiring that at least one of the two sub-propositions be satisfied
     */
    case class Or(a: Proposition, b: Proposition) extends Proposition

    /**
     * A proposition requiring that the given sub-proposition *not* be satisfied
     */
    case class Not(a: Proposition) extends Proposition
  }

  object Contextual {

    /**
     * A proposition requiring the blockchain to have reached a certain height to be unlocked
     */
    case class HeightLock(height: Long) extends Proposition

    /**
     * A proposition requiring certain characteristics about the spending transactions inputs/outputs
     * @param boxes a list of (box, location) tuples indicating an expected box at the expected location of the
     *              spending transaction
     */
    case class RequiredBoxState(boxes: List[(Box, BoxLocation)]) extends Proposition
  }

}

sealed abstract class BoxLocation

object BoxLocations {
  case class Input(index: Short) extends BoxLocation
  case class Output(index: Short) extends BoxLocation
}
