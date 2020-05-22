package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.Try

//TODO change codeBoxIds to codeBoxUUIDs
case class ExecutionBox(override val proposition: PublicKey25519Proposition,
                                override val nonce: Long,
                                override val value: UUID,
                                stateBoxUUIDs: Seq[UUID], //List of uuids of state boxes from ProgramBoxRegistry
                                codeBoxIds: Seq[Array[Byte]]
                                ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"

  override lazy val id: Array[Byte] = ExecutionBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "uuid" -> value.asJson,
    "stateBoxUUIDs" -> stateBoxUUIDs.asJson,
    "codeBoxIds" -> codeBoxIds.map(cb => Base58.encode(cb)).asJson,
    "nonce" -> nonce.toString.asJson,
  ).asJson

}

object ExecutionBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "execution".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeCodeBox: Decoder[ExecutionBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    uuid <- c.downField("uuid").as[UUID]
    stateBoxUUIDs <- c.downField("stateBoxUUIDs").as[Seq[UUID]]
    nonce <- c.downField("nonce").as[Long]
    codeBoxIds <- c.downField("codeBoxIds").as[Seq[String]]
  } yield {
//      val preparedPubKey = proposition.map(t => Base58.decode(t).get).toSet
//      val prop = MofNProposition(1, preparedPubKey)
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    val codeBoxes: Seq[Array[Byte]] = codeBoxIds.map(cb => Base58.decode(cb).get)
    ExecutionBox(prop, nonce, uuid, stateBoxUUIDs, codeBoxes)
  }
}

object ExecutionBoxSerializer {

  def toBytes(obj: ExecutionBox): Array[Byte] = {

    val boxType = "ExecutionBox"
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      obj.proposition.pubKeyBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.value.getMostSignificantBits),
      Longs.toByteArray(obj.value.getLeastSignificantBits),
      Ints.toByteArray(obj.stateBoxUUIDs.length),
      obj.stateBoxUUIDs.foldLeft(Array[Byte]()) {
        (arr, x) =>
          arr ++ Bytes.concat(
            Longs.toByteArray(x.getMostSignificantBits),
            Longs.toByteArray(x.getLeastSignificantBits)
          )
      },
      Ints.toByteArray(obj.codeBoxIds.length),
      obj.codeBoxIds.foldLeft(Array[Byte]()) {
        (arr, x) => arr ++ x
      }
    )
  }

  def parseBytes(obj: Array[Byte]): Try[ExecutionBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2 * Longs.BYTES)))
    takenBytes += Longs.BYTES * 2

    val stateBoxUUIDsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    var stateBoxUUIDs = Seq[UUID]()
    for (_ <- 1 to stateBoxUUIDsLength) {
      val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
        Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + Longs.BYTES * 2)))
      takenBytes += Longs.BYTES * 2
      stateBoxUUIDs = stateBoxUUIDs :+ uuid
    }

    val codeBoxIdsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    val codeBoxIds: Seq[Array[Byte]] = (0 until codeBoxIdsLength).map { i =>
      val id: Array[Byte] = obj.slice(takenBytes + i * (4 * Longs.BYTES), takenBytes + (i + 1) * (4 * Longs.BYTES))
      id
    }
    takenBytes += Longs.BYTES * 4 * codeBoxIdsLength

    ExecutionBox(prop, nonce, uuid, stateBoxUUIDs, codeBoxIds)
  }
}
