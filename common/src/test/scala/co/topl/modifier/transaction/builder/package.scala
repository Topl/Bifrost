package co.topl.modifier.transaction

import cats.implicits._
import cats.Foldable
import cats.data.NonEmptyChain
import co.topl.models.{Transaction => TetraTransaction}
import co.topl.modifier.box.{TokenBox, TokenValueHolder}

package object builder {

  def arbitOutputsAmount(from: List[TetraTransaction.ArbitOutput]): BigInt = from.foldMap(_.value.data)
  def arbitOutputsAmount(from: NonEmptyChain[TetraTransaction.ArbitOutput]): BigInt = arbitOutputsAmount(from.toList)

  def polyOutputsAmount(from: List[TetraTransaction.PolyOutput]): BigInt = from.foldMap(_.value.data)
  def polyOutputsAmount(from: NonEmptyChain[TetraTransaction.PolyOutput]): BigInt = polyOutputsAmount(from.toList)

  def boxesAmount(from: List[TokenBox[TokenValueHolder]]): BigInt =
    from.foldMap(box => BigInt(box.value.quantity.toByteArray))

  def boxesAmount(from: NonEmptyChain[TokenBox[TokenValueHolder]]): BigInt = boxesAmount(from.toList)
}
