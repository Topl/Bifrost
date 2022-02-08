package co.topl.codecs.json.attestation.keyManagement

import co.topl.attestation.Address
import co.topl.attestation.keyManagement.{Keyfile, KeyfileCurve25519, KeyfileEd25519}
import co.topl.codecs.binary._
import co.topl.codecs.json.attestation.AttestationJsonCodecs
import co.topl.codecs.json.valuetypes._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

trait KeyManagementJsonCodecs extends AttestationJsonCodecs {

  implicit val keyfileCurve25519JsonEncoder: Encoder[KeyfileCurve25519] = { kf: KeyfileCurve25519 =>
    Map(
      "crypto" -> Map(
        "cipher"       -> "aes-256-ctr".asJson,
        "cipherParams" -> Map("iv" -> kf.iv.encodeAsBase58.asJson).asJson,
        "cipherText"   -> kf.cipherText.encodeAsBase58.asJson,
        "kdf"          -> "scrypt".asJson,
        "kdfSalt"      -> kf.salt.encodeAsBase58.asJson,
        "mac"          -> kf.mac.encodeAsBase58.asJson
      ).asJson,
      "address" -> kf.address.asJson
    ).asJson
  }

  implicit def keyfileCurve25519JsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[KeyfileCurve25519] =
    (c: HCursor) =>
      for {
        address    <- c.downField("address").as[Address]
        cipherText <- c.downField("crypto").downField("cipherText").as[Base58Data]
        mac        <- c.downField("crypto").downField("mac").as[Base58Data]
        salt       <- c.downField("crypto").downField("kdfSalt").as[Base58Data]
        iv         <- c.downField("crypto").downField("cipherParams").downField("iv").as[Base58Data]
      } yield {
        implicit val netPrefix: NetworkPrefix = address.networkPrefix
        KeyfileCurve25519(
          address,
          cipherText.encodeAsBytes,
          mac.encodeAsBytes,
          salt.encodeAsBytes,
          iv.encodeAsBytes
        )
      }

  implicit val keyfileEd25519JsonEncoder: Encoder[KeyfileEd25519] = { kf: KeyfileEd25519 =>
    Map(
      "crypto" -> Map(
        "cipher"       -> "aes-256-ctr".asJson,
        "cipherParams" -> Map("iv" -> kf.iv.encodeAsBase58).asJson,
        "cipherText"   -> kf.cipherText.encodeAsBase58.asJson,
        "kdf"          -> "scrypt".asJson,
        "kdfSalt"      -> kf.salt.encodeAsBase58.asJson,
        "mac"          -> kf.mac.encodeAsBase58.asJson
      ).asJson,
      "address" -> kf.address.asJson
    ).asJson
  }

  implicit def keyfileEd25519JsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[KeyfileEd25519] =
    (c: HCursor) =>
      for {
        address    <- c.downField("address").as[Address]
        cipherText <- c.downField("crypto").downField("cipherText").as[Base58Data]
        mac        <- c.downField("crypto").downField("mac").as[Base58Data]
        salt       <- c.downField("crypto").downField("kdfSalt").as[Base58Data]
        iv         <- c.downField("crypto").downField("cipherParams").downField("iv").as[Base58Data]
      } yield {
        implicit val netPrefix: NetworkPrefix = address.networkPrefix
        KeyfileEd25519(address, cipherText.encodeAsBytes, mac.encodeAsBytes, salt.encodeAsBytes, iv.encodeAsBytes)
      }

  implicit def keyfileJsonEncoder[KF <: Keyfile[_]]: Encoder[KF] = {
    case kfc: KeyfileCurve25519 => keyfileCurve25519JsonEncoder(kfc)
    case kfe: KeyfileEd25519    => keyfileEd25519JsonEncoder(kfe)
  }
}
