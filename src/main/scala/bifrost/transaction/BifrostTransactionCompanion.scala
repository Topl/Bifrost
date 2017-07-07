package bifrost.transaction

import com.google.common.primitives.{Bytes, Doubles, Ints, Longs}
import bifrost.contract._
import bifrost.transaction.AssetRedemption.ConversionTransaction
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.ContractTransactionCompanion.typeBytes
import bifrost.transaction.Role.Role
import bifrost.transaction.box.{ContractBox, ContractBoxSerializer, ReputationBox}
import io.circe.{HCursor, Json, JsonObject, ParsingFailure}
import io.circe.optics.JsonPath._
import io.circe.syntax._
import io.circe.parser._
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object BifrostTransactionCompanion extends Serializer[BifrostTransaction] {

  override def toBytes(m: BifrostTransaction): Array[Byte] = m match {
    case c: ContractTransaction => ContractTransactionCompanion.toBytes(c)
    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
    case r: ProfileTransaction => ProfileTransactionCompanion.toBytes(r)
    case ar: AssetRedemption => AssetRedemptionCompanion.toBytes(ar)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    typeStr match {
      case "ContractTransaction" => ContractTransactionCompanion.parseBytes(bytes).get
      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).get
      case "ProfileTransaction" => ProfileTransactionCompanion.parseBytes(bytes).get
      case "AssetRedemption" => AssetRedemptionCompanion.parseBytes(bytes).get
    }
  }

}

object ContractTransactionCompanion extends Serializer[ContractTransaction] {

  val typeBytes = "ContractTransaction".getBytes

  val prefixBytes = Ints.toByteArray(typeBytes.length) ++ typeBytes

  override def toBytes(m: ContractTransaction): Array[Byte] = {
    prefixBytes ++
      (m match {
        case cc: ContractCreation => ContractCreationCompanion.toChildBytes(cc)
        case cme: ContractMethodExecution => ContractMethodExecutionCompanion.toChildBytes(cme)
        case ccomp: ContractCompletion => ContractCompletionCompanion.toChildBytes(ccomp)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    /* Grab the rest of the bytes, which should begin similarly (with sub-type) */
    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.take(Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES,  Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "ContractCreation" => ContractCreationCompanion.parseBytes(newBytes).get
      case "ContractMethodExecution" => ContractMethodExecutionCompanion.parseBytes(newBytes).get
      case "ContractCompletion" => ContractCompletionCompanion.parseBytes(newBytes).get
    }
  }

  def commonToBytes(m: ContractTransaction): Array[Byte] = {

    // Used to reduce overall size in the default case where publickeys are the same across multiple maps
    val keySeq = (m.signatures.keySet ++ m.fees.keySet ++ m.parties.values).map(v => ByteArrayWrapper(v.pubKeyBytes)).toSeq.zipWithIndex
    val keyMapping: Map[ByteArrayWrapper, Int] = keySeq.toMap

    Bytes.concat(
      Longs.toByteArray(m.timestamp),
      Ints.toByteArray(m.signatures.size),
      Ints.toByteArray(m.parties.size),
      Ints.toByteArray(m.preFeeBoxes.size),
      Ints.toByteArray(m.fees.size),
      Ints.toByteArray(keyMapping.size),
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ b._1.data),
      m.parties.foldLeft(Array[Byte]())((a, b) => {
        a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._2.pubKeyBytes))) ++ (b._1 match {
        case Role.Producer => Ints.toByteArray(0)
        case Role.Investor => Ints.toByteArray(1)
        case Role.Hub => Ints.toByteArray(2)
      })}),
      m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1.pubKeyBytes))) ++ b._2.bytes),
      m.preFeeBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1.pubKeyBytes))) ++ Ints.toByteArray(b._2.length) ++
        b._2.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2))),
      m.fees.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1.pubKeyBytes))) ++ Longs.toByteArray(b._2))
    )
  }

  //noinspection ScalaStyle
  def commonParseBytes(bytes: Array[Byte]): (
    Map[Role, PublicKey25519Proposition],
    Map[PublicKey25519Proposition, Signature25519],
    Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    Map[PublicKey25519Proposition, Long],
    Long
    ) = {

    var numReadBytes = 0

    val timestamp: Long = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))

    numReadBytes += Longs.BYTES

    val Array(sigLength: Int, partiesLength: Int, feePreBoxLength: Int, feesLength: Int, keyMappingSize: Int) = (0 until 5).map { i =>
      Ints.fromByteArray(bytes.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 5*Ints.BYTES

    val keyMapping: Map[Int, PublicKey25519Proposition] = (0 until keyMappingSize).map { i =>
      i -> PublicKey25519Proposition(bytes.slice(numReadBytes + i*Constants25519.PubKeyLength, numReadBytes + (i + 1)*Constants25519.PubKeyLength))
    }.toMap

    numReadBytes += keyMappingSize*Constants25519.PubKeyLength

    val roleTypes = Map[Int, Role.Role](
      0 -> Role.Producer,
      1 -> Role.Investor,
      2 -> Role.Hub
    )

    val parties: Map[Role.Role, PublicKey25519Proposition] = (0 until partiesLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + 2*i*Ints.BYTES, numReadBytes + (2*i + 1)*Ints.BYTES))
      val roleInt = Ints.fromByteArray(bytes.slice(numReadBytes + (2*i + 1)*Ints.BYTES, numReadBytes + 2*(i + 1)*Ints.BYTES))
      roleTypes(roleInt) -> keyMapping(pkInt)
    }.toMap

    numReadBytes += partiesLength*(Ints.BYTES*2)

    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until sigLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + i*(Ints.BYTES + Curve25519.SignatureLength), numReadBytes + i*Curve25519.SignatureLength + (i + 1)*Ints.BYTES))
      val sigBytes = bytes.slice(numReadBytes + (i + 1)*Ints.BYTES + i*Curve25519.SignatureLength, numReadBytes + (i + 1)*Ints.BYTES + (i + 1)*Curve25519.SignatureLength)
      keyMapping(pkInt) -> Signature25519(sigBytes)
    }.toMap

    numReadBytes += sigLength*(Ints.BYTES + Curve25519.SignatureLength)

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = (0 until feePreBoxLength).map { i =>
      var bytesSoFar = 0
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES))
      bytesSoFar += Ints.BYTES

      val length = Ints.fromByteArray(bytes.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES))
      bytesSoFar += Ints.BYTES

      val preBoxes = (0 until length).map { j =>
        var innerBytesSoFar = j*2*Longs.BYTES
        val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + bytesSoFar + innerBytesSoFar, numReadBytes + bytesSoFar + innerBytesSoFar + Longs.BYTES))
        val amount = Longs.fromByteArray(bytes.slice(numReadBytes + bytesSoFar + innerBytesSoFar + Longs.BYTES, numReadBytes + bytesSoFar + innerBytesSoFar + 2*Longs.BYTES))
        nonce -> amount
      }
      numReadBytes += bytesSoFar + length*2*Longs.BYTES
      keyMapping(pkInt) -> preBoxes
    }.toMap

    val fees: Map[PublicKey25519Proposition, Long] = (0 until feesLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + i*(Ints.BYTES + Longs.BYTES), numReadBytes + i*(Ints.BYTES + Longs.BYTES) + Ints.BYTES))
      val fee = Longs.fromByteArray(bytes.slice(numReadBytes + i*(Ints.BYTES + Longs.BYTES) + Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES + (i + 1)*Longs.BYTES))
      keyMapping(pkInt) -> fee
    }.toMap

    (parties, signatures, feePreBoxes, fees, timestamp)
  }
}

object TransferTransactionCompanion extends Serializer[TransferTransaction] {
  val typeBytes = "TransferTransaction".getBytes

  val prefixBytes = Ints.toByteArray(typeBytes.length) ++ typeBytes

  override def toBytes(m: TransferTransaction): Array[Byte] = {
    prefixBytes ++
      (m match {
        case sc: PolyTransfer => PolyTransferCompanion.toChildBytes(sc)
        case ac: ArbitTransfer => ArbitTransferCompanion.toChildBytes(ac)
        case at: AssetTransfer => AssetTransferCompanion.toChildBytes(at)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "PolyTransfer" => PolyTransferCompanion.parseBytes(newBytes).get
      case "ArbitTransfer" => ArbitTransferCompanion.parseBytes(newBytes).get
      case "AssetTransfer" => AssetTransferCompanion.parseBytes(newBytes).get
    }
  }
}

object ProfileTransactionCompanion extends Serializer[ProfileTransaction] {
  override def toBytes(m: ProfileTransaction): Array[Byte] = {
    val typeBytes = "ProfileTransaction".getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      m.json.toString().getBytes()
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProfileTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val json: Json = parse(new String(bytesWithoutType)).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor

    val from = PublicKey25519Proposition(Base58.decode(root.from.string.getOption(json).get).get)
    val fee = root.fee.long.getOption(json).get
    val timestamp = root.timestamp.long.getOption(json).get
    val signature = Signature25519(Base58.decode(root.signature.string.getOption(json).get).get)
    val keyValues: Map[String, String] = cursor.downField("keyValues").as[Map[String, String]].getOrElse(Map())

    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }
}

//noinspection ScalaStyle
object ContractCreationCompanion extends Serializer[ContractCreation] {

  override def toBytes(m: ContractCreation): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(m)
  }

  def toChildBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

    val agreementBytes = AgreementCompanion.toBytes(m.agreement)

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(m.preInvestmentBoxes.length),
      m.preInvestmentBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2)),
      Longs.toByteArray(agreementBytes.length),
      agreementBytes,
      ContractTransactionCompanion.commonToBytes(m)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val numPreInvestmentBoxes: Int = Ints.fromByteArray(bytesWithoutType.slice(0, Ints.BYTES))

    numReadBytes = Ints.BYTES

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numPreInvestmentBoxes).map { i =>
      val nonce = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + 2*i*Longs.BYTES, numReadBytes + (2*i + 1)*Longs.BYTES))
      val value = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + (2*i + 1)*Longs.BYTES, numReadBytes + 2*(i + 1)*Longs.BYTES))
      nonce -> value
    }

    numReadBytes += 2*numPreInvestmentBoxes*Longs.BYTES

    val agreementLength: Long = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES))

    numReadBytes += Longs.BYTES

    val agreement = AgreementCompanion.parseBytes(bytesWithoutType.slice(numReadBytes, numReadBytes + agreementLength.toInt)).get

    numReadBytes += agreementLength.toInt

    val (parties: Map[Role, PublicKey25519Proposition],
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ContractTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes, bytesWithoutType.length))

    ContractCreation(agreement, preInvestmentBoxes, parties, signatures, feePreBoxes, fees, timestamp)
  }

}

object ContractMethodExecutionCompanion extends Serializer[ContractMethodExecution] {

  override def toBytes(cme: ContractMethodExecution): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(cme)
  }

  def toChildBytes(cme: ContractMethodExecution): Array[Byte] = {
    val typeBytes = "ContractMethodExecution".getBytes

     Bytes.concat(
        /* First two arguments MUST STAY */
        Ints.toByteArray(typeBytes.length),
        typeBytes,
        Ints.toByteArray(cme.methodName.getBytes.length),
        Ints.toByteArray(cme.parameters.noSpaces.getBytes.length),
        Ints.toByteArray(cme.contractBox.bytes.length),
        cme.methodName.getBytes,
        cme.parameters.noSpaces.getBytes,
        cme.contractBox.bytes,
        ContractTransactionCompanion.commonToBytes(cme)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractMethodExecution] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))

    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(methodNameLength: Int, parameterJsonLength: Int, contractBoxLength: Int) = (0 until 3).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(i*Ints.BYTES, (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes = 3*Ints.BYTES

    val methodName = new String(bytesWithoutType.slice(numReadBytes, numReadBytes + methodNameLength))

    numReadBytes += methodNameLength

    val parameters: Json = parse(new String(bytesWithoutType.slice(numReadBytes, numReadBytes + parameterJsonLength))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    numReadBytes += parameterJsonLength

    val contractBox: ContractBox = ContractBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes, numReadBytes + contractBoxLength)).get

    numReadBytes += contractBoxLength

    val (parties: Map[Role, PublicKey25519Proposition],
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ContractTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes, bytesWithoutType.length))

    ContractMethodExecution(contractBox, methodName, parameters, parties, signatures, feePreBoxes, fees, timestamp)
  }

}

object ContractCompletionCompanion extends Serializer[ContractCompletion] {

  override def toBytes(cc: ContractCompletion): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(cc)
  }

  def toChildBytes(cc: ContractCompletion): Array[Byte] = {
    val typeBytes = "ContractCompletion".getBytes

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(cc.producerReputation.length),
      Ints.toByteArray(cc.contractBox.bytes.length),
      cc.producerReputation.foldLeft(Array[Byte]())((a, b) =>
        a ++ b.proposition.pubKeyBytes ++ Longs.toByteArray(b.nonce) ++ doubleToByteArray(b.value._1) ++ doubleToByteArray(b.value._2)
      ),
      cc.contractBox.bytes,
      ContractTransactionCompanion.commonToBytes(cc)
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[ContractCompletion] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)


    val Array(reputationLength: Int, contractBoxLength: Int) = (0 until 2).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(i*Ints.BYTES, (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes = 2*Ints.BYTES

    val producerReputation: IndexedSeq[ReputationBox] = (0 until reputationLength) map { i =>
      val proposition = PublicKey25519Proposition(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES),
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength
      ))

      val nonce = Longs.fromByteArray(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength,
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength + Longs.BYTES
      ))

      val Array(alpha: Double, beta: Double) = (0 until 2).map { j =>
        byteArrayToDouble(
          bytesWithoutType.slice(
            numReadBytes + (i + 1)*(Constants25519.PubKeyLength + Longs.BYTES) + 2*i*Doubles.BYTES + j*Doubles.BYTES,
            numReadBytes + (i + 1)*(Constants25519.PubKeyLength + Longs.BYTES) + 2*i*Doubles.BYTES + (j + 1)*Doubles.BYTES
          )
        )
      }.toArray

      ReputationBox(proposition, nonce, (alpha, beta))
    }

    numReadBytes += reputationLength*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES)

    val contractBox: ContractBox = ContractBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes, numReadBytes + contractBoxLength)).get

    numReadBytes += contractBoxLength

    val (parties: Map[Role, PublicKey25519Proposition],
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ContractTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes, bytesWithoutType.length))

    ContractCompletion(contractBox, producerReputation, parties, signatures, feePreBoxes, fees, timestamp)
  }

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
      res +=  ((x(i) & 0xff).toLong << ((7 - i) * 8))
    }
    java.lang.Double.longBitsToDouble(res)
  }

}

object AgreementCompanion extends Serializer[Agreement] {

  override def toBytes(a: Agreement): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.contractExpirationTime),
      Longs.toByteArray(a.contractEffectiveTime),
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      Ints.toByteArray(a.assetCode.getBytes.length),
      a.assetCode.getBytes,
      a.terms.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {

    val Array(contractExpirationTime: Long, contractEffectiveTime: Long, termsLength: Long) = (0 until 3).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 3*Longs.BYTES

    val numStrBytes = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))

    numBytesRead += Ints.BYTES

    val assetCode: String = new String(bytes.slice(numBytesRead, numBytesRead + numStrBytes))

    numBytesRead += numStrBytes

    val terms: AgreementTerms = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(x) => throw new Exception("AgreementTerm json not properly formatted")
      case Right(x) => x.as[AgreementTerms] match {
        case Left(_) => throw new Exception("Agreement terms json was malformed")
        case Right(a: AgreementTerms) => a
      }
    }

    Agreement(terms, assetCode, contractEffectiveTime, contractExpirationTime)
  }
}

trait TransferSerializer {
  def transferToBytes(tx: TransferTransaction, txType: String): Array[Byte] = {
      val typeBytes = txType.getBytes
    
      Bytes.concat(
        Ints.toByteArray(typeBytes.length),
        typeBytes,
        Longs.toByteArray(tx.fee),
        Longs.toByteArray(tx.timestamp),
        Ints.toByteArray(tx.signatures.length),
        Ints.toByteArray(tx.from.length),
        Ints.toByteArray(tx.to.length),
        tx.signatures.foldLeft(Array[Byte]())((a,b) => a ++ b.bytes),
        tx.from.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
        tx.to.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
      )
  }
  
  def conversionToBytes(ct: ConversionTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes
    
    // concatenate map keys to reduce size when assetCodes are the same
    val keySeq = (ct.totalAssetBoxes.keySet ++ ct.assetsToReturn.keySet ++
      ct.assetTokensToRedeem.keySet ++ ct.conversionSignatures.keySet).toSeq.zipWithIndex
    val keyMapping: Map[(String, PublicKey25519Proposition), Int] = keySeq.toMap
    
    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(ct.fee),
      Longs.toByteArray(ct.timestamp),
      Ints.toByteArray(ct.totalAssetBoxes.size),
      Ints.toByteArray(ct.assetsToReturn.size),
      Ints.toByteArray(ct.assetTokensToRedeem.size),
      Ints.toByteArray(ct.conversionSignatures.size),
      Ints.toByteArray(keyMapping.size),
      keySeq.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(b._1._1.getBytes.length) ++ b._1._1.getBytes ++ b._1._2.pubKeyBytes),
      ct.totalAssetBoxes.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(totAssets => totAssets._1.pubKeyBytes ++ Longs.toByteArray(totAssets._2))
      ),
      ct.assetsToReturn.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetReturn => assetReturn._1.pubKeyBytes ++ Longs.toByteArray(assetReturn._2))
      ),
      ct.assetTokensToRedeem.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetRedeem => assetRedeem._1.pubKeyBytes ++ Longs.toByteArray(assetRedeem._2))
      ),
      ct.conversionSignatures.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(_.signature)
      )
    )
  }

  def parametersParseBytes(bytes: Array[Byte]): (IndexedSeq[(PublicKey25519Proposition, Long)],
                                                 IndexedSeq[(PublicKey25519Proposition, Long)],
                                                 IndexedSeq[Signature25519], Long, Long) = {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2*Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice( numBytesRead + 2*Longs.BYTES, numBytesRead + 2*Longs.BYTES + Ints.BYTES))

    numBytesRead += 2*Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2*Ints.BYTES))

    numBytesRead += 2*Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(numBytesRead + i * Curve25519.SignatureLength, numBytesRead + (i + 1) * Curve25519.SignatureLength))
    }

    numBytesRead += sigLength*Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1)*elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    (from, to, signatures, fee, timestamp)
  }
}

object PolyTransferCompanion extends Serializer[PolyTransfer] with TransferSerializer {

  override def toBytes(sc: PolyTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(sc)
  }

  def toChildBytes(sc: PolyTransfer): Array[Byte] = {
    transferToBytes(sc, "PolyTransfer")
  }

  override def parseBytes(bytes: Array[Byte]): Try[PolyTransfer] = Try {
    val params = parametersParseBytes(bytes)
    PolyTransfer(params._1, params._2, params._3, params._4, params._5)
  }
}

object AssetTransferCompanion extends Serializer[AssetTransfer] with TransferSerializer {

  override def toBytes(at: AssetTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(at)
  }

  def toChildBytes(at: AssetTransfer): Array[Byte] = {
    transferToBytes(at, "AssetTransfer") ++
      at.hub.pubKeyBytes ++
      at.assetCode.getBytes ++
      Ints.toByteArray(at.assetCode.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetTransfer] = Try {
    val params = parametersParseBytes(bytes)

    val assetCodeLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val assetCode: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen, bytes.length - Ints.BYTES)
    )
    val hub: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Constants25519.PubKeyLength, bytes.length - Ints.BYTES - assetCodeLen)
    )

    AssetTransfer(params._1, params._2, params._3, hub, assetCode, params._4, params._5)
  }
}

object ArbitTransferCompanion extends Serializer[ArbitTransfer] with TransferSerializer {

  override def toBytes(ac: ArbitTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(ac)
  }

  def toChildBytes(ac: ArbitTransfer): Array[Byte] = {
    transferToBytes(ac, "ArbitTransfer")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ArbitTransfer] = Try {
    val params = parametersParseBytes(bytes)
    ArbitTransfer(params._1, params._2, params._3, params._4, params._5)
  }
}

object AssetRedemptionCompanion extends Serializer[AssetRedemption] {
  override def toBytes(ac: AssetRedemption): Array[Byte] = {
    val typeBytes = "AssetRedemption".getBytes

    // Used to reduce overall size in the default case where assetcodes are the same across multiple maps
    val keySeq = (ac.signatures.keySet ++ ac.availableToRedeem.keySet ++ ac.remainderAllocations.keySet).toSeq.zipWithIndex
    val keyMapping: Map[String, Int] = keySeq.toMap

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(ac.fee),
      Longs.toByteArray(ac.timestamp),
      Ints.toByteArray(ac.signatures.size),
      Ints.toByteArray(ac.availableToRedeem.size),
      Ints.toByteArray(ac.remainderAllocations.size),
      Ints.toByteArray(keyMapping.size),
      ac.hub.pubKeyBytes,
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(b._1.getBytes.length) ++ b._1.getBytes),
      ac.signatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(_.signature)
      ),
      ac.availableToRedeem.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(box => box._1.pubKeyBytes ++ Longs.toByteArray(box._2))
      ),
      ac.remainderAllocations.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(alloc => alloc._1.pubKeyBytes ++ Longs.toByteArray(alloc._2))
      )
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[AssetRedemption] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)


    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    numReadBytes = 2*Longs.BYTES

    val Array(sigLength: Int, availableToRedeemLength: Int, amountsLength: Int, keyMappingSize: Int) = (0 until 4).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 4*Ints.BYTES

    val hub = PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val keyMapping: Map[Int, String] = (0 until keyMappingSize).map { i =>
      val strLen = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
      val assetId = new String(bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + strLen))

      numReadBytes += Ints.BYTES + strLen
      i -> assetId
    }.toMap

    val signatures: Map[String, IndexedSeq[Signature25519]] = (0 until sigLength).map { i =>

      val assetId:String = keyMapping(Ints.fromByteArray(
        bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)
      ))

      val numSigs = Ints.fromByteArray(
        bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES)
      )

      val sigs: IndexedSeq[Signature25519] = (0 until numSigs).map { j =>
        Signature25519(
          bytesWithoutType.slice(
            numReadBytes + j*Curve25519.SignatureLength + 2*Ints.BYTES,
            numReadBytes + (j + 1)*Curve25519.SignatureLength  + 2*Ints.BYTES
          )
        )
      }

      numReadBytes += 2*Ints.BYTES + numSigs*Curve25519.SignatureLength
      assetId -> sigs
    }.toMap

    val availableToRedeem: Map[String, IndexedSeq[(PublicKey25519Proposition, Nonce)]] = (0 until availableToRedeemLength).map { _ =>
      var bytesSoFar = 0
      val assetId = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

      bytesSoFar = Ints.BYTES

      val boxesLength = Ints.fromByteArray(
        bytesWithoutType.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES)
      )

      bytesSoFar += Ints.BYTES

      val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

      val boxes: IndexedSeq[(PublicKey25519Proposition, Nonce)] =  (0 until boxesLength).map { j =>
        val prop = PublicKey25519Proposition(
          bytesWithoutType.slice(
            numReadBytes + bytesSoFar + j*chunkSize,
            numReadBytes + bytesSoFar + j*chunkSize + Constants25519.PubKeyLength
          )
        )

        val nonceStart = numReadBytes + bytesSoFar + j*chunkSize + Constants25519.PubKeyLength
        val nonce = Longs.fromByteArray(bytesWithoutType.slice(nonceStart, nonceStart + Longs.BYTES))

        prop -> nonce
      }

      bytesSoFar += boxesLength*chunkSize
      numReadBytes += bytesSoFar

      assetId -> boxes
    }.toMap

    val remainderAllocations: Map[String, IndexedSeq[(PublicKey25519Proposition, Long)]] = (0 until amountsLength).map { _ =>
      val assetId = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

      val allocationLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES))

      val startPosition = numReadBytes + 2*Ints.BYTES
      val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

      val allocationSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until allocationLength).map { i =>
        val prop = PublicKey25519Proposition(
          bytesWithoutType.slice(startPosition + i*chunkSize, startPosition + i*chunkSize + Constants25519.PubKeyLength)
        )
        val amount = Longs.fromByteArray(
          bytesWithoutType.slice(startPosition + i*chunkSize + Constants25519.PubKeyLength, startPosition + (i + 1)*chunkSize)
        )
        prop -> amount
      }

      numReadBytes += 2*Ints.BYTES + allocationLength*chunkSize
      assetId -> allocationSeq
    }.toMap

    AssetRedemption(availableToRedeem, remainderAllocations, signatures, hub, fee, timestamp)
  }
}

object ConversionTransactionCompanion extends Serializer[ConversionTransaction] with TransferSerializer {
  
  override def toBytes(ct: ConversionTransaction): Array[Byte] = {
    conversionToBytes(ct, "ConversionTransaction")
  }
  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[ConversionTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)
    
    //read in byte stream to array for fee and timestamp
    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray
    
    numReadBytes = 2*Longs.BYTES
    
    val Array(totalAssetLength: Int, assetReturnLength: Int, assetRedeemLength: Int, sigLength: Int, keyMappingSize: Int) =
      (0 until 5).map { i =>
        Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
      }.toArray
    
    numReadBytes += 5*Ints.BYTES
    
    val keyMapping: Map[Int, (String, PublicKey25519Proposition)] = (0 until keyMappingSize).map { i =>
      val ahLen = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
      val asset = new String(bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + ahLen))
      
      numReadBytes += Ints.BYTES + ahLen
      
      val hub = PublicKey25519Proposition(
        bytesWithoutType.slice(
          numReadBytes,
          numReadBytes + Constants25519.PubKeyLength
        )
      )
      
      numReadBytes += Constants25519.PubKeyLength
      
      i -> (asset, hub)
    }.toMap
    
    val totalAssets: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Nonce)]] =
      (0 until totalAssetLength).map { _ =>
        var bytesSoFar = 0
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))
        
        bytesSoFar = Ints.BYTES
        
        val boxesLength = Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES)
        )
        
        bytesSoFar += Ints.BYTES
        
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES
        
        val boxes: IndexedSeq[(PublicKey25519Proposition, Nonce)] = (0 until boxesLength).map { j =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              numReadBytes + bytesSoFar + j*chunkSize,
              numReadBytes + bytesSoFar + j*chunkSize + Constants25519.PubKeyLength
            )
          )
          
          val nonceStart = numReadBytes + bytesSoFar + j*chunkSize + Constants25519.PubKeyLength
          val nonce = Longs.fromByteArray(bytesWithoutType.slice(nonceStart, nonceStart + Longs.BYTES))
          
          prop -> nonce
        }
        
        bytesSoFar += boxesLength*chunkSize
        numReadBytes += bytesSoFar
        
        assetHub -> boxes
      }.toMap
    
    val assetReturn: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]] =
      (0 until assetReturnLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))
        
        val retLength = Ints.fromByteArray(bytesWithoutType.slice(
          numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES))
        
        val startPosition = numReadBytes + 2*Ints.BYTES
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES
        
        val retSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until retLength).map { i =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              startPosition + i*chunkSize,
              startPosition + i*chunkSize + Constants25519.PubKeyLength)
          )
          
          val amount = Longs.fromByteArray(
            bytesWithoutType.slice(
              startPosition + i*chunkSize + Constants25519.PubKeyLength,
              startPosition + (i + 1)*chunkSize)
          )
          
          prop -> amount
        }
  
        numReadBytes += 2*Ints.BYTES + retLength*chunkSize
        assetHub -> retSeq
      }.toMap
    
    val assetRedeem: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]] =
      (0 until assetRedeemLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))
    
        val redeemLength = Ints.fromByteArray(bytesWithoutType.slice(
          numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES))
    
        val startPosition = numReadBytes + 2*Ints.BYTES
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES
    
        val redeemSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until redeemLength).map { i =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              startPosition + i*chunkSize,
              startPosition + i*chunkSize + Constants25519.PubKeyLength)
          )
      
          val amount = Longs.fromByteArray(
            bytesWithoutType.slice(
              startPosition + i*chunkSize + Constants25519.PubKeyLength,
              startPosition + (i + 1)*chunkSize)
          )
      
          prop -> amount
        }
    
        numReadBytes += 2*Ints.BYTES + redeemLength*chunkSize
        assetHub -> redeemSeq
      }.toMap
    
    val signatures: Map[(String, PublicKey25519Proposition), IndexedSeq[Signature25519]] =
      (0 until sigLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))
        
        val numSigs = Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES)
        )
        
        val sigs: IndexedSeq[Signature25519] = (0 until numSigs).map { j =>
          Signature25519(
            bytesWithoutType.slice(
              numReadBytes + j*Curve25519.SignatureLength + 2*Ints.BYTES,
              numReadBytes + (j + 1)*Curve25519.SignatureLength + 2*Ints.BYTES
            )
          )
        }
        
        numReadBytes += 2*Ints.BYTES + numSigs*Curve25519.SignatureLength
        assetHub -> sigs
      }.toMap
    
    ConversionTransaction(totalAssets, assetReturn, assetRedeem, signatures, fee, timestamp)
  }
}
