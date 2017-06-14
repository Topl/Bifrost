package bifrost.contract

import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

case class AgreementTerms(pledge: Long, xrate: BigDecimal, share: ShareFunction, fulfilment: FulfilmentFunction){

  lazy val json: Json = Map(
    "pledge" -> pledge.asJson,
    "xrate" -> xrate.asJson,
    "share" -> share.json,
    "fulfilment" -> fulfilment.json
  ).asJson

  override def toString: String = s"AgreementTerms(${json.toString})"

}

object AgreementTerms {

  implicit val decodeTerms: Decoder[AgreementTerms] = (c: HCursor) => for {
    pledge <- c.downField("pledge").as[Long]
    xrate <- c.downField("xrate").as[BigDecimal]
    share <- c.downField("share").as[ShareFunction]
    fulfilment <- c.downField("fulfilment").as[FulfilmentFunction]
  } yield AgreementTerms(pledge, xrate, share, fulfilment)

  // def validate(terms: AgreementTerms): Try[Unit] = Try {
  //   require(terms.pledge > 0)
  //   require(terms.xrate > 0)
  // }
}