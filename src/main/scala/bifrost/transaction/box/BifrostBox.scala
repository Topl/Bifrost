package bifrost.transaction.box

import com.google.common.primitives.{Bytes, Doubles, Ints, Longs}
import bifrost.scorexMod.GenericBox
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import io.circe.{Decoder, HCursor, Json}
import io.circe.parser._
import io.circe.syntax._
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.serialization.Serializer
import bifrost.transaction.box.proposition.{Constants25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.utils.Booleans

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class BifrostBox(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                          val nonce: Long,
                          value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = BifrostBox

  override def serializer: Serializer[BifrostBox] = BifrostBoxSerializer

  // lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey: ProofOfKnowledgeProposition[PrivateKey25519] = proposition

  val typeOfBox: String

  val json: Json

  override def equals(obj: Any): Boolean = obj match {
    case acc: BifrostBox => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }


  override def hashCode(): Int = proposition.hashCode()
}


object BifrostBoxSerializer extends Serializer[BifrostBox] {

  override def toBytes(obj: BifrostBox): Array[Byte] = obj match {
    case p: PolyBox => PolyBoxSerializer.toBytes(p)
    case a: ArbitBox => ArbitBoxSerializer.toBytes(a)
    case as: AssetBox => AssetBoxSerializer.toBytes(as)
    case c: ContractBox => ContractBoxSerializer.toBytes(c)
    case profileb: ProfileBox => ProfileBoxSerializer.toBytes(profileb)
    case repBox: ReputationBox => ReputationBoxSerializer.toBytes(repBox)
    case sb: StateBox => StateBoxSerializer.toBytes(sb)
    case cb: CodeBox => CodeBoxSerializer.toBytes(cb)
    case _ => throw new Exception("Unanticipated BifrostBox type")
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostBox] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))
    
    typeStr match {
      case "ArbitBox" => ArbitBoxSerializer.parseBytes(bytes)
      case "AssetBox" => AssetBoxSerializer.parseBytes(bytes)
      case "PolyBox" => PolyBoxSerializer.parseBytes(bytes)
      case "ContractBox" => ContractBoxSerializer.parseBytes(bytes)
      case "ProfileBox" => ProfileBoxSerializer.parseBytes(bytes)
      case "ReputationBox" => ReputationBoxSerializer.parseBytes(bytes)
      case "StateBox" => StateBoxSerializer.parseBytes(bytes)
      case "CodeBox" => CodeBoxSerializer.parseBytes(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}

case class PolyBox(override val proposition: PublicKey25519Proposition,
                   override val nonce: Long,
                   override val value: Long) extends BifrostPublic25519NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Poly"
}

object PolyBoxSerializer extends Serializer[PolyBox] with NoncedBoxSerializer {

  def toBytes(obj: PolyBox): Array[Byte] = {
    noncedBoxToBytes(obj, "PolyBox")
  }

  override def parseBytes(bytes: Array[Byte]): Try[PolyBox] = Try {
    val params = noncedBoxParseBytes(bytes)
    PolyBox(params._1, params._2, params._3)
  }

}

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends BifrostPublic25519NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Arbit"
}

object ArbitBoxSerializer extends Serializer[ArbitBox] with NoncedBoxSerializer {

  def toBytes(obj: ArbitBox): Array[Byte] = {
    noncedBoxToBytes(obj, "ArbitBox")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ArbitBox] = Try {
    val params = noncedBoxParseBytes(bytes)
    ArbitBox(params._1, params._2, params._3)
  }

}

case class AssetBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    amount: Long,
                    assetCode: String,
                    issuer: PublicKey25519Proposition,
                    data: String) extends BifrostPublic25519NoncedBox(proposition, nonce, amount) {
  override lazy val typeOfBox: String = "Asset"

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "value" -> value.toString.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "data" -> data.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}

object AssetBoxSerializer extends Serializer[AssetBox] with NoncedBoxSerializer {

  def toBytes(obj: AssetBox): Array[Byte] = {
    noncedBoxToBytes(obj, "AssetBox") ++
      obj.issuer.pubKeyBytes ++
      obj.assetCode.getBytes ++
      Ints.toByteArray(obj.assetCode.getBytes.length) ++
      obj.data.getBytes ++
      Ints.toByteArray(obj.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {

    val params = noncedBoxParseBytes(bytes)

    val dataLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))

    val assetLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - Ints.BYTES - dataLen,
      bytes.length - Ints.BYTES - dataLen))
    val asset: String = new String(bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen,
      bytes.length - (2 * Ints.BYTES) - dataLen))

    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen - Constants25519.PubKeyLength,
                  bytes.length - (2 * Ints.BYTES) - dataLen - assetLen)
    )

    AssetBox(params._1, params._2, params._3, asset, issuer, data)
  }
}

case class ContractBox(proposition: MofNProposition,
                       override val nonce: Long,
                       value: Json) extends BifrostBox(proposition, nonce, value) {

  val typeOfBox = "ContractBox"
  lazy val id: Array[Byte] = FastCryptographicHash(
    MofNPropositionSerializer.toBytes(proposition) ++
      Longs.toByteArray(nonce) ++
      value.noSpaces.getBytes
  )

  override lazy val json: Json = Map(
    "type" -> "Contract".asJson,
    "id" -> Base58
      .encode(id)
      .asJson,
    "proposition" -> proposition
      .setOfPubKeyBytes
      .toList
      .map(Base58.encode)
      .sorted
      .map(_.asJson)
      .asJson,
    "value" -> value.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson

}

object ContractBox {
  implicit val decodeContractBox: Decoder[ContractBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[Seq[String]]
    value <- c.downField("value").as[Json]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = proposition.map(t => Base58.decode(t).get).toSet
    val prop = MofNProposition(1, preparedPubKey)
    ContractBox(prop, nonce, value)
  }
}

object ContractBoxSerializer extends Serializer[ContractBox] {

  def toBytes(obj: ContractBox): Array[Byte] = {

    val boxType = "ContractBox"

    Ints.toByteArray(boxType.getBytes.length) ++
      boxType.getBytes ++
      MofNPropositionSerializer.toBytes(obj.proposition) ++
      Longs.toByteArray(obj.nonce) ++
      Ints.toByteArray(obj.value.noSpaces.getBytes.length) ++
      obj.value.noSpaces.getBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    var numReadBytes = Ints.BYTES + typeLen

    val numOfPk = Ints.fromByteArray(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + 2 * Ints.BYTES))
    val endIndex = numReadBytes + 2 * Ints.BYTES + numOfPk * Constants25519.PubKeyLength
    val proposition = MofNPropositionSerializer.parseBytes(bytes.slice(numReadBytes, endIndex)).get
    numReadBytes = endIndex

    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))

    numReadBytes += Longs.BYTES

    val valueLen = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))

    val value = parse(new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + valueLen))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    ContractBox(proposition, nonce, value)
  }

}

/**
  *
  * @param proposition
  * @param nonce : place holder for now. Make it always zero
  * @param value
  * @param key   : Name of the profile attribute you wish to use for the box
  */
case class ProfileBox(proposition: PublicKey25519Proposition,
                      override val nonce: Long,
                      value: String,
                      key: String) extends BifrostBox(proposition, nonce, value) {

  lazy val id: Array[Byte] = ProfileBox.idFromBox(proposition, key)

  val typeOfBox = "ProfileBox"

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> "Profile".asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.asJson,
    "key" -> key.asJson
  ).asJson
}

object ProfileBox {

  val acceptableKeys = Set("role")
  val acceptableRoleValues = Set("investor", "hub", "producer")

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, field: String): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ field.getBytes)

  implicit val decodeProfileBox: Decoder[ProfileBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("value").as[String]
    field <- c.downField("key").as[String]
  } yield {
    val pubkey = PublicKey25519Proposition(Base58.decode(proposition).get)
    ProfileBox(pubkey, 0L, value, field)
  }
}

object ProfileBoxSerializer extends Serializer[ProfileBox] {

  def toBytes(obj: ProfileBox): Array[Byte] = {

    val boxType = "ProfileBox"

    Ints.toByteArray(boxType.getBytes.length) ++ boxType.getBytes ++
      obj.proposition.pubKeyBytes ++
      Ints.toByteArray(obj.value.getBytes.length) ++ obj.value.getBytes ++
      Ints.toByteArray(obj.key.getBytes.length) ++ obj.key.getBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProfileBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    var numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val valueLen = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val value = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + valueLen))

    numReadBytes += Ints.BYTES + valueLen
    val fieldLen = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val field = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + fieldLen))
    ProfileBox(pk, 0L, value, field)
  }

}

case class ReputationBox(override val proposition: PublicKey25519Proposition,
                         override val nonce: Long,
                         value: (Double, Double)) extends BifrostBox(proposition, nonce, value) {
  val typeOfBox: String = "Reputation"
  val id = ReputationBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> "Reputation".asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}

object ReputationBox {

  val byteSize = Constants25519.PubKeyLength + Longs.BYTES + 2 * Doubles.BYTES

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "reputation".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeReputationBox: Decoder[ReputationBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("value").as[(Double, Double)]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    ReputationBox(prop, nonce, value)
  }
}

object ReputationBoxSerializer extends Serializer[ReputationBox] {

  def doubleToByteArray(x: Double): Array[Byte] = {
    val l = java.lang.Double.doubleToLongBits(x)
    val a = Array.fill(8)(0.toByte)
    for (i <- 0 to 7) a(i) = ((l >> ((7 - i) * 8)) & 0xff).toByte
    a
  }

  def byteArrayToDouble(x: Array[scala.Byte]): Double = {
    var i = 0
    var res = 0.toLong
    for (i <- 0 to 7) {
      res += ((x(i) & 0xff).toLong << ((7 - i) * 8))
    }
    java.lang.Double.longBitsToDouble(res)
  }

  def toBytes(obj: ReputationBox): Array[Byte] = {

    val boxType = "ReputationBox"

    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      Longs.toByteArray(obj.nonce),
      doubleToByteArray(obj.value._1),
      doubleToByteArray(obj.value._2),
      obj.proposition.pubKeyBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ReputationBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    val newBytes = bytes.slice(Ints.BYTES + typeLen, bytes.length)

    val nonce = Longs.fromByteArray(newBytes.take(Longs.BYTES))
    val Array(alpha: Double, beta: Double) = (0 until 2).map {
      i => byteArrayToDouble(newBytes.slice(Longs.BYTES + i * Doubles.BYTES, Longs.BYTES + (i + 1) * Doubles.BYTES))
    }.toArray
    val proposition = PublicKey25519Proposition(
      newBytes.slice(Longs.BYTES + 2 * Doubles.BYTES, Longs.BYTES + 2 * Doubles.BYTES + Constants25519.PubKeyLength)
    )

    ReputationBox(proposition, nonce, (alpha, beta))
  }
}

case class StateBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    value: Seq[String], // List of Strings of JS Variable Declarations
                    mutabilityFlag: Boolean,
                   ) extends BifrostBox(proposition, nonce, value) {

  val typeOfBox: String = "StateBox"

  val id = StateBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> "State".asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.asJson,
    "nonce" -> nonce.toString.asJson,
    "mutabilityFlag" -> mutabilityFlag.asJson
  ).asJson

}

object StateBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "state".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeStateBox: Decoder[StateBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("value").as[Seq[String]]
    nonce <- c.downField("nonce").as[Long]
    mutabilityFlag <- c.downField("mutabilityFlag").as[Boolean]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    StateBox(prop, nonce, value, mutabilityFlag)
  }

}

object StateBoxSerializer {

  def toBytes(obj: StateBox): Array[Byte] = {
    val boxType = "StateBox"
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      Longs.toByteArray(obj.nonce),
      Booleans.toByteArray(obj.mutabilityFlag),
      Ints.toByteArray(obj.value.length),
      obj.value.foldLeft(Array[Byte]()) {
        (arr, x) => arr ++ Bytes.concat(
          Ints.toByteArray(x.getBytes().length),
          x.getBytes()
        )
      },
      obj.proposition.pubKeyBytes
    )
  }

  def parseBytes(obj: Array[Byte]): Try[StateBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    assert(boxType == "StateBox") // no need to continue decoding if it's gibberish

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val mutabilityFlag = Booleans.fromByteArray(obj.slice(takenBytes, takenBytes + 1))
    takenBytes += 1

    val valueLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    var value = Seq[String]()
    for (_ <- 1 to valueLength) {
      val l = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES
      value = value :+ new String(obj.slice(takenBytes, takenBytes + l))
      takenBytes += l
    }
    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    StateBox(prop, nonce, value, mutabilityFlag)
  }

}

case class CodeBox(override val proposition: PublicKey25519Proposition,
                   override val nonce: Long,
                   value: Seq[String], // List of strings of JS functions
                  ) extends BifrostBox(proposition, nonce, value) {

  val typeOfBox: String = "CodeBox"

  val id = CodeBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> "Code".asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.asJson,
    "nonce" -> nonce.toString.asJson,
  ).asJson

}

object CodeBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "code".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeCodeBox: Decoder[CodeBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("value").as[Seq[String]]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    CodeBox(prop, nonce, value)
  }

}

object CodeBoxSerializer {

  def toBytes(obj: CodeBox): Array[Byte] = {
    val boxType = "CodeBox"
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      Longs.toByteArray(obj.nonce),
      Ints.toByteArray(obj.value.length),
      obj.value.foldLeft(Array[Byte]()) {
        (arr, x) => arr ++ Bytes.concat(
          Ints.toByteArray(x.getBytes().length),
          x.getBytes()
        )
      },
      obj.proposition.pubKeyBytes
    )
  }

  def parseBytes(obj: Array[Byte]): Try[CodeBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    //TODO Check this, it may not fail gracefully
    assert(boxType == "CodeBox") // no need to continue decoding if it's gibberish

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val valueLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    var value = Seq[String]()
    for (_ <- 1 to valueLength) {
      val l = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES
      value = value :+ new String(obj.slice(takenBytes, takenBytes + l))
      takenBytes += l
    }

    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    CodeBox(prop, nonce, value)
  }
}
