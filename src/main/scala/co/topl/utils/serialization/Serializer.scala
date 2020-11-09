package co.topl.utils.serialization

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.ArbitTransfer

import scala.util.Try

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {

  def serialize ( obj: T, w: W ): Unit

  def parse ( r: R ): TFamily

  def parseTry ( r: R ): Try[TFamily] = {
    Try(parse(r))
  }
}
