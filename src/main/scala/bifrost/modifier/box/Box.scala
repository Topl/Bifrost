package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519}
import com.google.common.primitives.{Bytes, Doubles, Ints, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.parser._
import io.circe.syntax._
import bifrost.serialization.Serializer
import bifrost.modifier.box.proposition.{Constants25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class Box(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                   val nonce: Long,
                   value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = Box

  override def serializer: Serializer[Box] = BoxSerializer

  // lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey: ProofOfKnowledgeProposition[PrivateKey25519] = proposition

  val typeOfBox: String

  val json: Json

  override def equals(obj: Any): Boolean = obj match {
    case acc: Box => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object BoxSerializer extends Serializer[Box] {

  override def toBytes(obj: Box): Array[Byte] = obj match {
    case p: PolyBox => PolyBoxSerializer.toBytes(p)
    case a: ArbitBox => ArbitBoxSerializer.toBytes(a)
    case as: AssetBox => AssetBoxSerializer.toBytes(as)
    case sb: StateBox => StateBoxSerializer.toBytes(sb)
    case cb: CodeBox => CodeBoxSerializer.toBytes(cb)
    case eb: ExecutionBox => ExecutionBoxSerializer.toBytes(eb)
    case _ => throw new Exception("Unanticipated Box type")
  }

  override def parseBytes(bytes: Array[Byte]): Try[Box] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    typeStr match {
      case "ArbitBox" => ArbitBoxSerializer.parseBytes(bytes)
      case "AssetBox" => AssetBoxSerializer.parseBytes(bytes)
      case "PolyBox" => PolyBoxSerializer.parseBytes(bytes)
      case "StateBox" => StateBoxSerializer.parseBytes(bytes)
      case "CodeBox" => CodeBoxSerializer.parseBytes(bytes)
      case "ExecutionBox" => ExecutionBoxSerializer.parseBytes(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}



case class StateBox(override val proposition: PublicKey25519Proposition,
                            override val nonce: Long,
                            override val value: UUID,
                            state: Json //  JSON representation of JS Variable Declarations
                            ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "StateBox"

  override lazy val id = StateBox.idFromBox(proposition, nonce)

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

case class CodeBox(override val proposition: PublicKey25519Proposition,
                           override val nonce: Long,
                           override val value: UUID,
                           code: Seq[String], // List of strings of JS functions
                           interface: Map[String, Seq[String]]
                           ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "CodeBox"

  override lazy val id = CodeBox.idFromBox(proposition, nonce)

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

//TODO change codeBoxIds to codeBoxUUIDs
case class ExecutionBox(override val proposition: PublicKey25519Proposition,
                                override val nonce: Long,
                                override val value: UUID,
                                stateBoxUUIDs: Seq[UUID], //List of uuids of state boxes from ProgramBoxRegistry
                                codeBoxIds: Seq[Array[Byte]]
                                ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"

  override lazy val id = ExecutionBox.idFromBox(proposition, nonce)

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