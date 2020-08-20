package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.Longs
import io.circe.parser
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58

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

  // TODO: Jing - Check if this is used anywhere
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

object StateBoxSerializer extends BifrostSerializer[StateBox] {

  override def serialize(obj: StateBox, w: Writer): Unit = {
    w.putByteString("StateBox")
    ProgramBoxSerializer.serialize(obj, w)

    /* state: Json, JSON representation of JS Variable Declarations */
    val state: Array[Byte] = obj.state.noSpaces.getBytes
    w.putUInt(state.length)
    w.putBytes(state)
  }

  override def parse(r: Reader): StateBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    /* state: Json, JSON representation of JS Variable Declarations */
    val stateLength: Int = r.getUInt().toIntExact
    val stateString: String = new String(r.getBytes(stateLength))
    val state: Json = parser.parse(stateString) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    StateBox(programBox.proposition, programBox.nonce, programBox.value, state)
  }

// TODO: Jing - remove
//
//  override def toBytes(obj: StateBox): Array[Byte] = {
//    val boxType = "StateBox"
//    Bytes.concat(
//      Ints.toByteArray(boxType.getBytes.length),
//      boxType.getBytes,
//      Longs.toByteArray(obj.nonce),
//      Longs.toByteArray(obj.value.getMostSignificantBits),
//      Longs.toByteArray(obj.value.getLeastSignificantBits),
//      Ints.toByteArray(obj.state.noSpaces.getBytes.length),
//      obj.state.noSpaces.getBytes,
//      obj.proposition.pubKeyBytes
//    )
//  }
//
//  override def parseBytes(obj: Array[Byte]): Try[StateBox] = Try {
//    var takenBytes = 0
//
//    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
//    takenBytes += Ints.BYTES
//
//    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
//    takenBytes += boxTypeLength
//
//    require(boxType == "StateBox")
//
//    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
//    takenBytes += Longs.BYTES
//
//    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
//      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2*Longs.BYTES)))
//    takenBytes += Longs.BYTES*2
//
//    val stateLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
//    takenBytes += Ints.BYTES
//
//    val state: Json = parser.parse(new String(obj.slice(takenBytes, takenBytes + stateLength))) match {
//      case Left(f) => throw f
//      case Right(j: Json) => j
//    }
//    takenBytes += stateLength
//
//    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
//    takenBytes += Constants25519.PubKeyLength
//
//    StateBox(prop, nonce, uuid, state)
//  }
}
