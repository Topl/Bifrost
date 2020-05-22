package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.Try

case class CodeBox(override val proposition: PublicKey25519Proposition,
                           override val nonce: Long,
                           override val value: UUID,
                           code: Seq[String], // List of strings of JS functions
                           interface: Map[String, Seq[String]]
                           ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "CodeBox"

  override lazy val id: Array[Byte] = CodeBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "nonce" -> nonce.toString.asJson,
    "uuid" -> value.asJson,
    "code" -> code.asJson,
    "interface" -> interface.map(ci => ci._1 -> ci._2.asJson).asJson
  ).asJson

}

object CodeBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "code".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeCodeBox: Decoder[CodeBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    uuid <- c.downField("uuid").as[UUID]
    code <- c.downField("code").as[Seq[String]]
    interface <- c.downField("interface").as[Map[String, Seq[String]]]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    CodeBox(prop, nonce, uuid, code, interface)
  }

}

object CodeBoxSerializer {

  def toBytes(obj: CodeBox): Array[Byte] = {
    val boxType = "CodeBox"
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.value.getMostSignificantBits),
      Longs.toByteArray(obj.value.getLeastSignificantBits),
      Ints.toByteArray(obj.code.length),
      obj.code.foldLeft(Array[Byte]())(
        (a, b) => a ++ Bytes.concat(
          Ints.toByteArray(b.getBytes().length), b.getBytes())),
      Ints.toByteArray(obj.interface.size),
      obj.interface.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(b._1.length) ++ b._1.getBytes ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(s => Bytes.concat(Ints.toByteArray(s.length) ++ s.getBytes))),
      obj.proposition.pubKeyBytes
    )
  }

  def parseBytes(obj: Array[Byte]): Try[CodeBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2 * Longs.BYTES)))
    takenBytes += 2 * Longs.BYTES

    val codeLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    var code = Seq[String]()
    for (_ <- 1 to codeLength) {
      val l = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES
      code = code :+ new String(obj.slice(takenBytes, takenBytes + l))
      takenBytes += l
    }

    val interfaceLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    val interface: Map[String, Seq[String]] = (0 until interfaceLength).map{ _ =>

      val methodNameLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES

      val methodName = new String(obj.slice(takenBytes, takenBytes + methodNameLength))
      takenBytes += methodNameLength

      val paramsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES

      val params: Seq[String] = (0 until paramsLength).map { _ =>
        val strLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
        takenBytes += Ints.BYTES

        val str = new String(obj.slice(takenBytes, takenBytes + strLength))
        takenBytes += strLength
        str
      }
      methodName -> params
    }.toMap

    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    CodeBox(prop, nonce, uuid, code, interface)
  }
}
