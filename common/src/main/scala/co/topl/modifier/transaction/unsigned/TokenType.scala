package co.topl.modifier.transaction.unsigned

sealed trait TokenType

object TokenTypes {
  case object Assets extends TokenType
  case object Polys extends TokenType
  case object Arbits extends TokenType
}

object TokenType {

  def getTypePrefix(token: TokenType): Byte =
    token match {
      case TokenTypes.Assets => 3.toByte
      case TokenTypes.Polys  => 2.toByte
      case TokenTypes.Arbits => 1.toByte
    }

  def getTypeString(token: TokenType): String =
    token match {
      case TokenTypes.Polys  => "PolyTransfer"
      case TokenTypes.Arbits => "ArbitTransfer"
      case TokenTypes.Assets => "AssetTransfer"
    }
}
