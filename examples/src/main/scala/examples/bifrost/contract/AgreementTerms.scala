package examples.bifrost.contract

import io.circe.Json
import io.circe.syntax._

class AgreementTerms(pledge: BigDecimal,
                     xrate: BigDecimal,
                     share: ShareFunction,
                     fulfilment: FulfilmentFunction){

  lazy val json: Json = Map(
    "pledge" -> Json.fromBigDecimal(pledge),
    "xrate" -> Json.fromBigDecimal(xrate),
    "share" -> Map(
      "functionType" -> Json.fromString(share.functionType),
      "points" -> Json.arr(share.points.map(p => Json.arr(
          Json.fromDouble(p._1).get,
          Json.arr(Json.fromDouble(p._2._1).get, Json.fromDouble(p._2._2).get, Json.fromDouble(p._2._3).get)
        )
      ):_*)
    ).asJson,
    "fulfilment" -> Map(
      "functionType" -> Json.fromString(share.functionType),
      "points" -> Json.arr(
        fulfilment.points.map(p => Json.arr(Json.fromLong(p._1), Json.fromDouble(p._2).get)):_*
      )
    ).asJson
  ).asJson

  override def toString: String = s"AgreementTerms(${json.noSpaces})"

}