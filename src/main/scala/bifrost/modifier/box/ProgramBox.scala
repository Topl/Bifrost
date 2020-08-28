package bifrost.modifier.box

import java.util.UUID

import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

class ProgramBox(override val proposition: PublicKey25519Proposition,
                 override val nonce: Long,
                 override val value: UUID) extends Box(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "ProgramBox"

  lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.toString.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}

object ProgramBoxSerializer extends BifrostSerializer[ProgramBox] {

  override def serialize(obj: ProgramBox, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)
    w.putLong(obj.nonce)

    // The SignificantBits could be negative longs
    w.putLong(obj.value.getMostSignificantBits)
    w.putLong(obj.value.getLeastSignificantBits)
  }

  override def parse(r: Reader): ProgramBox = {
    new ProgramBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      /* A UUID represents a 128-bit value, the two longs are the most and least significant 64 bits of this UUID */
      new UUID(r.getLong(), r.getLong())
    )
  }
}
