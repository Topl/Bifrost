package co.topl.modifier.transaction.unsigned

import co.topl.modifier.box.{AssetValue, SimpleValue, TokenValueHolder}
import co.topl.utils.Int128

sealed trait TokenValue

object TokenValues {
  case class AssetTokens(value: AssetValue) extends TokenValue
  case class PolyTokens(value: SimpleValue) extends TokenValue
  case class ArbitTokens(value: SimpleValue) extends TokenValue
}

object TokenValue {

  def getTokenValueHolder(from: TokenValue): TokenValueHolder =
    from match {
      case TokenValues.AssetTokens(value) => value
      case TokenValues.PolyTokens(value)  => value
      case TokenValues.ArbitTokens(value) => value
    }

  def getIntValue(from: TokenValue): Int128 =
    from match {
      case TokenValues.AssetTokens(value) => value.quantity
      case TokenValues.PolyTokens(value)  => value.quantity
      case TokenValues.ArbitTokens(value) => value.quantity
    }
}
