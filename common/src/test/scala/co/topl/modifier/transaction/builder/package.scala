package co.topl.modifier.transaction

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.models.{Box => TetraBox, Transaction => TetraTransaction}
import co.topl.modifier.box.{TokenBox, TokenValueHolder}

package object builder {

  def arbitOutputsAmount(from: List[TetraTransaction.Output]): BigInt =
    from.collect { case TetraTransaction.Output(_, value: TetraBox.Values.Arbit, _) =>
      value.value.data
    }.sum

  def polyOutputsAmount(from: List[TetraTransaction.Output]): BigInt =
    from.collect { case TetraTransaction.Output(_, value: TetraBox.Values.Poly, _) =>
      value.value.data
    }.sum

  def boxesAmount(from: List[TokenBox[TokenValueHolder]]): BigInt =
    from.foldMap(box => BigInt(box.value.quantity.toByteArray))

  def boxesAmount(from: NonEmptyChain[TokenBox[TokenValueHolder]]): BigInt = boxesAmount(from.toList)
}
