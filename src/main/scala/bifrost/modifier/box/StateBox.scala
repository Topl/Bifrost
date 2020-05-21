package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.parser.parse
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class StateBox(override val proposition: PublicKey25519Proposition,
                            override val nonce: Long,
                            override val value: UUID,
                            state: Json //  JSON representation of JS Variable Declarations
                            ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "StateBox"

  override lazy val id: Array[Byte] = StateBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "uuid" -> value.asJson,
    "state" -> state.asJson,
    "nonce" -> nonce.toString.asJson,
  ).asJson

}

object StateBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "state".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeStateBox: Decoder[StateBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("uuid").as[UUID]
    state <- c.downField("state").as[Json]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    StateBox(prop, nonce, value, state)
  }

}

object StateBoxSerializer {

  def toBytes(obj: StateBox): Array[Byte] = {
    val boxType = "StateBox"
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.value.getMostSignificantBits),
      Longs.toByteArray(obj.value.getLeastSignificantBits),
      Ints.toByteArray(obj.state.noSpaces.getBytes.length),
      obj.state.noSpaces.getBytes,
      obj.proposition.pubKeyBytes
    )
  }

  def parseBytes(obj: Array[Byte]): Try[StateBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2*Longs.BYTES)))
    takenBytes += Longs.BYTES*2

    val stateLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    val state: Json = parse(new String(obj.slice(takenBytes, takenBytes + stateLength))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }
    takenBytes += stateLength

    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    StateBox(prop, nonce, uuid, state)
  }

}
