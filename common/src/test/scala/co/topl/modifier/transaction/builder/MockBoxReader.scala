package co.topl.modifier.transaction.builder

import cats.implicits._
import cats.data.NonEmptyChain
import co.topl.attestation.Address
import co.topl.modifier.box.{Box, BoxId, ProgramBox, TokenBox, TokenValueHolder}
import co.topl.modifier.{BoxReader, ProgramId}

import scala.reflect.ClassTag

class MockBoxReader(boxes: Map[Address, Seq[TokenBox[TokenValueHolder]]]) extends BoxReader[ProgramId, Address] {
  override def getBox(id: BoxId): Option[Box[_]] = boxes.values.flatten.find(_.id == id)

  override def getProgramBox[PBX <: ProgramBox: ClassTag](key: ProgramId): Option[PBX] = None

  override def getTokenBoxes(key: Address): Option[Seq[TokenBox[TokenValueHolder]]] = boxes.get(key)
}

object MockBoxReader {

  def empty: BoxReader[ProgramId, Address] = new MockBoxReader(Map.empty)

  def fromSeq(boxes: (Address, Seq[TokenBox[TokenValueHolder]])*): BoxReader[ProgramId, Address] =
    new MockBoxReader(Map(boxes: _*))

  def fromNec(boxes: (Address, NonEmptyChain[TokenBox[TokenValueHolder]])*): BoxReader[ProgramId, Address] =
    new MockBoxReader(Map(boxes.map(box => box._1 -> box._2.toList): _*))
}
