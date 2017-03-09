package examples.bifrost.transaction.contract

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

class AgreementTerms(pledge: BigDecimal,
                     xrate: BigDecimal,
                     share: Long => (BigDecimal, BigDecimal, BigDecimal),
                     fulfilment: Long => BigDecimal){

  lazy val json: Json = Map(
    "pledge" -> Json.fromBigDecimal(pledge),
    "xrate" -> Json.fromBigDecimal(xrate),
    "share" -> Json.fromString(Base58.encode(serialise(share))),
    "fulfilment" -> Json.fromString(Base58.encode(serialise(fulfilment)))
  ).asJson

  override def toString: String = s"AgreementTerms(${json.noSpaces})"

  private def serialise[A](f: A) : Array[Byte] = {
    val bo = new ByteArrayOutputStream()
    new ObjectOutputStream(bo).writeObject(f)
    bo.toByteArray
  }
}