package co.topl.codecs

import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.encode.Base58
import io.circe.syntax._
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scodec.bits.BitVector
import scodec.{Decoder => ScodecDecoder, Encoder => ScodecEncoder}

/**
 * Contains instances of the Circe `Encoder` and `Decoder` typeclasses.
 *
 * @example {{{
 * import co.topl.codecs.json._
 * import io.circe.syntax._
 *
 * val modifierId: ModifierId = ...
 *
 * // encode a value as JSON
 * modifierId.asJson
 *
 * // decode a value from a Circe `Json` object
 * val modifierIdJson: Json = ...
 * modifierIdJson.as[ModifierId]
 *
 * }}}
 */
package object json extends JsonCodecs {

  /**
   * Derives a Circe JSON `Encoder` instance from a Scodec `Encoder` instance.
   * The value will be encoded to its Base-58 string representation.
   * @param typeName the name of the encodable type for use in failure messages
   * @tparam T the type which can be encoded as Base-58
   * @return a Circe `Encoder` instance for type `T`
   */
  private[json] def deriveEncoderFromScodec[T: ScodecEncoder](typeName: String): Encoder[T] = value =>
    ScodecEncoder[T]
      .encode(value)
      .toEither
      .map(_.encodeAsBase58.asJson)
      .leftMap(err => s"failed to encode $typeName as binary data: $err")
      .getOrThrow()

  /**
   * Derives a Circe JSON `KeyEncoder` instance from a Scodec `Encoder` instance.
   * The value will be encoded to its Base-58 string representation.
   * @param typeName the name of the encodable type for use in failure messages
   * @tparam T the type which can be encoded as Base-58 data
   * @return a Circe `KeyEncoder` instance for type `T`
   */
  private[json] def deriveKeyEncoderFromScodec[T: ScodecEncoder](typeName: String): KeyEncoder[T] = value =>
    ScodecEncoder[T]
      .encode(value)
      .toEither
      .map(bits => Base58.encode(bits.toByteArray))
      .leftMap(err => s"failed to encode $typeName as binary data: $err")
      .getOrThrow()

  /**
   * Derives a Circe JSON `Decoder` instance from a Scodec `Decoder` instance.
   * The decoder will attempt to decode the JSON into a Base-58 string and then decode the bytes representation
   * of the Base-58 data into a type `T`.
   * @param typeName the name of the encodable type for use in failure messages
   * @tparam T the type which can be decoded from Base-58 data
   * @return a Circe `Decoder` instance for type `T`
   */
  private[json] def deriveDecoderFromScodec[T: ScodecDecoder](typeName: String): Decoder[T] =
    Decoder[Base58Data]
      .emap(data =>
        ScodecDecoder[T]
          .decodeValue(BitVector(data.encodeAsBytes))
          .toEither
          .leftMap(err => s"failed to decode $typeName from binary data: $err")
      )

  /**
   * Derives a Circe JSON `KeyDecoder` instance from a Scodec `Decoder` instance.
   * The decoder will attempt to decode the JSON into a Base-58 string and then decode the bytes representation
   * of the Base-58 data into a type `T`.
   * @param typeName the name of the encodable type for use in failure messages
   * @tparam T the type which can be decoded from Base-58 data
   * @return a Circe `KeyDecoder` instance for type `T`
   */
  private[json] def deriveKeyDecoderFromScodec[T: ScodecDecoder](typeName: String): KeyDecoder[T] =
    KeyDecoder[Base58Data]
      .map(data =>
        ScodecDecoder[T]
          .decodeValue(BitVector(data.encodeAsBytes))
          .toEither
          .leftMap(err => s"failed to decode $typeName from binary data: $err")
          .getOrThrow()
      )
}
