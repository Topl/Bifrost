package co.topl.nodeView.state

import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.box._

import scala.reflect.ClassTag

/**
 * A mock state reader which associates addresses with the boxes they own. Maintains a static set of boxes.
 * @param tokenBoxes the mapping of addresses with sets of owned token boxes
 * @param versionId the version representing the current state
 */
class MockStateReader(tokenBoxes: Map[Address, List[TokenBox[TokenValueHolder]]], versionId: ModifierId)
    extends StateReader[ProgramId, Address] {
  override def version: ModifierId = versionId

  override def getBox(id: BoxId): Option[Box[_]] = tokenBoxes.values.toList.flatten.find(_.id == id)

  override def getProgramBox[PBX <: ProgramBox: ClassTag](key: ProgramId): Option[PBX] = None

  override def getTokenBoxes(key: Address): Option[Seq[TokenBox[TokenValueHolder]]] = tokenBoxes.get(key).map(_.toList)

  override type NVCT = this.type
}

object MockStateReader {

  def apply(tokenBoxes: Map[Address, List[TokenBox[TokenValueHolder]]]): StateReader[ProgramId, Address] =
    new MockStateReader(tokenBoxes, ModifierId.empty)

  def apply(
    tokenBoxes: Map[Address, List[TokenBox[TokenValueHolder]]],
    version:    ModifierId
  ): StateReader[ProgramId, Address] =
    new MockStateReader(tokenBoxes, version)

  /**
   * Creates a [[MockStateReader]] with no boxes in state.
   * @return an instance of [[StateReader]] with no boxes in state
   */
  def empty: StateReader[ProgramId, Address] = new MockStateReader(Map.empty, ModifierId.empty)
}
