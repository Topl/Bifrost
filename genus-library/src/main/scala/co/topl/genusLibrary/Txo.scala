package co.topl.genusLibrary

import co.topl.genusLibrary
import co.topl.models.{Box, SpendingAddress}

//noinspection ScalaFileName
object TxoState extends Enumeration {
  type TxoState = Value

  val Spent: genusLibrary.TxoState.Value = Value
  val Unspent: genusLibrary.TxoState.Value = Value
  val Pending: genusLibrary.TxoState.Value = Value
}

import TxoState._

/**
 * The contents of this file should be generated from protobuf specs, but it is not yet.
 *
 * @param box the Box that represents the TxO
 * @param state The status of the box
 */
case class Txo(box: Box, state: TxoState, id: Box.Id, address: Option[SpendingAddress]) {}
