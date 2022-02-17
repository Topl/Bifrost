package co.topl.codecs.json.valuetypes

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data, Latin1Data}
import co.topl.utils.encode.{Base16, Base58}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import co.topl.utils.implicits._

import java.nio.charset.StandardCharsets

trait ValueTypesJsonCodecs {

  implicit private def nonEmptyChainShow[T: Show]: Show[NonEmptyChain[T]] =
    chain => chain.map(_.show).mkString_(", ")

  implicit val int128JsonEncoder: Encoder[Int128] = (x: Int128) => x.toString.asJson

  implicit val int128JsonDecoder: Decoder[Int128] = Decoder[String].map(s => Int128(s))

  implicit val base16JsonEncoder: Encoder[Base16Data] = (t: Base16Data) => Base16.encode(t.value).asJson

  implicit val base16JsonDecoder: Decoder[Base16Data] =
    Decoder[String].emap(
      Base16Data.validated(_).toEither.leftMap(err => s"failed to decode base-16 string: ${err.show}")
    )

  implicit val base16JsonKeyEncoder: KeyEncoder[Base16Data] = data => Base16.encode(data.value)

  implicit val base16JsonKeyDecoder: KeyDecoder[Base16Data] = Base16Data.validated(_).toOption

  implicit val base58JsonEncoder: Encoder[Base58Data] = data => Base58.encode(data.value).asJson

  implicit val base58JsonDecoder: Decoder[Base58Data] =
    Decoder[String].emap(
      Base58Data.validated(_).toEither.leftMap(err => s"failed to decode base-58 string: ${err.show}")
    )

  implicit val base58JsonKeyEncoder: KeyEncoder[Base58Data] = data => Base58.encode(data.value)

  implicit val base58JsonKeyDecoder: KeyDecoder[Base58Data] = Base58Data.validated(_).toOption

  implicit val latin1JsonEncoder: Encoder[Latin1Data] = (t: Latin1Data) =>
    new String(t.value, StandardCharsets.ISO_8859_1).asJson

  implicit val latin1JsonDecoder: Decoder[Latin1Data] =
    Decoder[String].emap(
      Latin1Data.validated(_).toEither.leftMap(err => s"failed to decode latin-1 string: ${err.show}")
    )

  implicit val latin1JsonKeyEncoder: KeyEncoder[Latin1Data] =
    data => new String(data.value, StandardCharsets.ISO_8859_1)

  implicit val latin1JsonKeyDecoder: KeyDecoder[Latin1Data] = Latin1Data.validated(_).toOption
}
