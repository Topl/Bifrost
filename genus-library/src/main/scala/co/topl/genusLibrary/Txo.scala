package co.topl.genusLibrary

import co.topl.genusLibrary
import co.topl.models.Box.Values.{Arbit, AssetV1, Empty, Poly}
import co.topl.models.utility.Base58
import co.topl.models.{Box, Int128, SpendingAddress}
import com.typesafe.scalalogging.Logger

//noinspection ScalaFileName
object TxoState extends Enumeration {
  type TxoState = Value

  val Spent: genusLibrary.TxoState.Value = Value
  val Unspent: genusLibrary.TxoState.Value = Value
  val Pending: genusLibrary.TxoState.Value = Value
}

import co.topl.genusLibrary.TxoState._

/**
 * The contents of this file should be generated from protobuf specs, but it is not yet.
 *
 * @param box the Box that represents the TxO
 * @param state The status of the box
 */
case class Txo(box: Box, state: TxoState, id: Box.Id, address: Option[SpendingAddress]) {
  import Txo._

  private def unsupported[T](v: Box.Value):T = throw GenusException(s"Encountered unsupported type of box value: $v")

  /**
   * Get the quantity in this TxO. Not all types of TxOs have a quantity.
   */
  def quantity: Option[Int128] = box.value match {
    case Empty => None
    case Poly(quantity) => Some(quantity)
    case Arbit(quantity) => Some(quantity)
    case AssetV1(quantity, _, _, _) => Some(quantity)
    case v:_ => unsupported(v)
  }

  /**
   * Return a string that identifies the type of asset in the TxO.
   * <ul>
   * <li>for Empty boxes this will be "EMPTY"</li>
   * <li>for Poly boxes this will be "LVL"</li>
   * <li>for Arbit boxes this will be "TOPL"</li>
   * <li>for AssetV1 boxes that will be <i>version</i>|<i>address</i>, where <i>version</i> is the hex value of the
   *     version byte and <i>address</i> is the base58 encoded address.
   * <li>for TAM2 boxes (not implemented yet) will be <i>group</i>:<i>series</i>, where <i>group</i> is the base58
   *     encoded id of the group constructor and <i>series</i> is the base58 encoded id of the series constructor.</li>
   * </ul>
   */
  def assetLabel: String = box.value match {
    case Empty => "EMPTY"
    case Poly(_) => "LVL"
    case Arbit(_) => "TOPL"
    case AssetV1(_, assetCode, -, _) =>
      assetCode.version.toHexString + "|" + Base58.encode(assetCode.issuer.typedEvidence.allBytes.toArray)
    case v:_ => unsupported(v)
  }

  def securityRoot: Option[Array[Byte]] = box.value match {
    case Empty | Poly(_) | Arbit(_) => None
    case AssetV1(_, _, securityRoot, _) => Some(securityRoot.data.toArray)
    case v:_ => unsupported(v)
  }

  def metadata: Option[Array[Byte]] = box.value match {
    case Empty | Poly(_) | Arbit(_) => None
    case AssetV1(_, _, _, metadata) => metadata.map(m => m.data.bytes)
    case v: _ => unsupported(v)
  }

}

object Txo {
  implicit private val logger: Logger = Logger(classOf[Genus])
}
