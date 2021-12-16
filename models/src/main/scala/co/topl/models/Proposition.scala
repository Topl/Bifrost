package co.topl.models

import io.estatico.newtype.macros.newsubtype

import scala.collection.immutable.ListSet
import scala.language.implicitConversions

sealed trait Proposition

object Propositions {

  case object PermanentlyLocked extends Proposition

  object Knowledge {
    case class Curve25519(key: VerificationKeys.Curve25519) extends Proposition
    case class Ed25519(key: VerificationKeys.Ed25519) extends Proposition
    case class ExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition
    case class HashLock(digest: Digest32) extends Proposition
  }

  object Compositional {
    case class Threshold(threshold: Int, propositions: ListSet[Proposition]) extends Proposition
    case class And(a: Proposition, b: Proposition) extends Proposition
    case class Or(a: Proposition, b: Proposition) extends Proposition
  }

  object Contextual {
    case class HeightLock(height: Long) extends Proposition
    //case class RequiredDionOutput(index: Int, address: DionAddress) extends Proposition
    case class RequiredBoxState(location: BoxLocation, boxes: List[(Int, Box[Box.Value])]) extends Proposition
  }

  object Example {
    case class EnumeratedInput(values: List[Int]) extends Proposition
  }

  object Script {
    case class JS(script: JS.JSScript) extends Proposition

    object JS {
      @newsubtype case class JSScript(value: String)
    }
  }

}

sealed abstract class BoxLocation
object BoxLocations {
  case object Input extends BoxLocation
  case object Output extends BoxLocation
}

