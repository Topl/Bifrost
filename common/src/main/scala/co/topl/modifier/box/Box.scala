package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.Nonce
import co.topl.utils.Identifier
import co.topl.codecs.binary.legacy.BifrostSerializer
import co.topl.codecs.binary.legacy.modifier.box.BoxSerializer
import com.google.common.primitives.Ints

/**
 * Created by Matthew on 4/11/2017.
 */
sealed abstract class Box[+T](val evidence: Evidence, val value: T, val nonce: Nonce) extends GenericBox[T] {

  lazy val id: BoxId = BoxId(this)

  @deprecated
  type M = Box[_]

  @deprecated
  override def serializer: BifrostSerializer[Box[_]] = BoxSerializer

//  @deprecated
//  override def toString: String = ???

  @deprecated
  override def hashCode(): Int = Ints.fromByteArray(bytes)

  @deprecated
  override def equals(obj: Any): Boolean = obj match {
    case box: Box[_] => bytes sameElements box.bytes
    case _           => false
  }
}

object Box {
  type Nonce = Long
  type BoxType = Byte

  def identifier(box: Box[_]): Identifier = box match {
    case _: ArbitBox     => ArbitBox.identifier.getId
    case _: PolyBox      => PolyBox.identifier.getId
    case _: AssetBox     => AssetBox.identifier.getId
    case _: ExecutionBox => ExecutionBox.identifier.getId
    case _: StateBox     => StateBox.identifier.getId
    case _: CodeBox      => CodeBox.identifier.getId
    case _               => throw new Exception("No matching identifier found")
  }
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

abstract class TokenBox[
  +T <: TokenValueHolder
](override val evidence: Evidence, override val nonce: Nonce, override val value: T)
    extends Box[T](evidence, value, nonce)

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

abstract class ProgramBox(
  override val evidence: Evidence,
  override val nonce:    Nonce,
  override val value:    ProgramId
) extends Box[ProgramId](evidence, value, nonce)
