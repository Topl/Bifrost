package bifrost.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import bifrost.transaction.PolyTransfer.Nonce
import bifrost.contract.{Contract, _}
import bifrost.scorexMod.GenericBoxTransaction
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import bifrost.transaction.box._
import bifrost.transaction.proof.MultiSignature25519
import bifrost.wallet.BWallet
import bifrost.transaction.ContractMethodExecutionCompanion
import bifrost.transaction.Role.Role
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}
import scala.util.parsing.json.JSONArray

sealed trait BifrostTransaction extends GenericBoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, BifrostBox] {
  val boxIdsToOpen: IndexedSeq[Array[Byte]]
}

sealed abstract class ContractTransaction extends BifrostTransaction

object Role extends Enumeration {
  type Role = Value
  val Producer: Role = Value("producer")
  val Investor: Role = Value("investor")
  val Hub: Role = Value("hub")
}

// 3 signatures FOR A SPECIFIC MESSAGE <agreement: Agreement>
// ContractCreation(agreement ++ nonce, IndexSeq(pk1, pk2, pk3), IndexSeq(sign1(agreement ++ nonce), sign2(agreement ++ nonce), sign3(agreement ++ nonce)) )
// validity check: decrypt[pk1] sign1(agreement) === agreement
// agreement specifies "executeBy" date
case class ContractCreation(agreement: Agreement,
                            parties: IndexedSeq[(Role, PublicKey25519Proposition)],
                            signatures: IndexedSeq[Signature25519],
                            fee: Long,
                            timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

  lazy val proposition = MofNProposition(1, parties.map(_._2.pubKeyBytes).toSet)

  // no boxes required for now -- will require reputation
  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq[Array[Byte]]()

  override lazy val unlockers: Traversable[BoxUnlocker[MofNProposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[MofNProposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Proof[MofNProposition] = MultiSignature25519(Set(signature))
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    AgreementCompanion.toBytes(agreement) ++
      parties.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
    // TODO check if this nonce is secure
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractCreation.nonceFromDigest(digest)

    val boxValue: Json = (parties.map(kv => kv._1.toString -> Base58.encode(kv._2.pubKeyBytes).asJson).toMap ++
      Map(
        "agreement" -> agreement.json,
        "storage" -> Map(
          "status" -> "initialized".asJson
        ).asJson,
        "lastUpdated" -> timestamp.asJson
      )
      ).asJson

    IndexedSeq(ContractBox(proposition, nonce, boxValue))
  }

  override lazy val json: Json = Map(
    "agreement" -> agreement.json,
    "parties" -> parties.map(kv => kv._1.toString -> Base58.encode(kv._2.pubKeyBytes).asJson ).asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    AgreementCompanion.toBytes(agreement),
    parties.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes)
  )

  override def toString: String = s"ContractCreation(${json.noSpaces})"
}

object ContractCreation {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def validate(tx: ContractCreation): Try[Unit] = Try {

    val outcome= Agreement.validate(tx.agreement)
    require(Agreement.validate(tx.agreement).isSuccess)

    require(tx.parties.size == tx.signatures.size && tx.parties.size == 3)
    require(tx.parties.map(_._1).toSet.size == 3) // Make sure there are exactly 3 unique roles
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.zip(tx.parties) forall { case (signature, (_, proposition)) =>
      signature.isValid(proposition, tx.messageToSign)
    })
  }

}

case class ContractMethodExecution(contractBox: ContractBox,
                                   party: (Role, PublicKey25519Proposition),
                                   methodName: String,
                                   parameters: Json,
                                   signature: Signature25519,
                                   fee: Long,
                                   timestamp: Long)
  extends ContractTransaction {

  override type M = ContractMethodExecution

  lazy val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

  lazy val proposition = MofNProposition(1,
    Set(
      contract.Producer.pubKeyBytes,
      contract.Hub.pubKeyBytes,
      contract.Investor.pubKeyBytes
    )
  )

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(contractBox.id)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = contractBox.id
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(Set(signature))
    }
  )

  lazy val hashNoNonces = FastCryptographicHash(
    contractBox.id ++
      methodName.getBytes ++
      party._2.pubKeyBytes ++
      parameters.noSpaces.getBytes ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )


  override lazy val newBoxes: Traversable[BifrostBox] = {
    // TODO check if this nonce is secure
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractMethodExecution.nonceFromDigest(digest)

    Contract.execute(contract, methodName)(party._2)(parameters.asObject.get) match {
      case Success(res) => res match {
        case Left(updatedContract) => IndexedSeq(ContractBox(proposition, nonce, updatedContract.json))
        case Right(_) => IndexedSeq(contractBox)
      }
      case Failure(_) => IndexedSeq(contractBox)
    }
  }

  override lazy val json: Json = Map(
    "contract" -> contract.json,
    "party" -> ( party._1.toString -> Base58.encode(party._2.pubKeyBytes).asJson ).asJson,
    "methodName" -> methodName.asJson,
    "parameters" -> parameters,
    "signature" -> Base58.encode(signature.bytes).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractMethodExecutionCompanion

  override lazy val messageToSign: Array[Byte] = hashNoNonces

  override def toString: String = s"ContractMethodExecution(${json.noSpaces})"

}

object ContractMethodExecution {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: ContractMethodExecution): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(MultiSignature25519(Set(tx.signature)).isValid(tx.contractBox.proposition, tx.messageToSign))
    require(tx.signature.isValid(tx.party._2, tx.messageToSign))
  }

}

case class ContractCompletion(contractBox: ContractBox,
                              reputation: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                              parties: IndexedSeq[(Role, PublicKey25519Proposition)],
                              signatures: IndexedSeq[Signature25519],
                              fee: Long,
                              timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCompletion

  lazy val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

  lazy val proposition = MofNProposition(1,
    Set(
      contract.Producer.pubKeyBytes,
      contract.Hub.pubKeyBytes,
      contract.Investor.pubKeyBytes
    )
  )

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(contractBox.id) ++ reputation.map(box => ReputationBox.idFromBox(box._1, box._2))

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = contractBox.id
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(signatures.toSet)
    }
  )

  lazy val hashNoNonces = FastCryptographicHash(
    contractBox.id ++
      parties.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )


  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractMethodExecution.nonceFromDigest(digest)

    /* Get yield */
    val input: Long = contract.agreement("terms").get.asObject.get("pledge").get.asNumber.get.toLong.get
    val output: Long = contract.storage("currentFulfillment").get.asObject.get("deliveredQuantity").get.asNumber.get.toLong.get
    val yieldRate = output.toDouble / input.toDouble

    /* Calculate alpha, beta changes */
    val w = input
    val alpha: Double = (w.toDouble / 1000)*(2*yieldRate - 1)
    val beta: Double = (w.toDouble / 1000)*(2 - yieldRate)

    /* Reputation adjustment for producer */
    val producerRep: ReputationBox = ReputationBox(contract.Producer, nonce, (alpha, beta))

    /* Change amount held in hub to account for actual delivery */
    //TODO add asset box: val amountAddedToHubAssets = output - input

    Seq(producerRep)
  }

  override lazy val json: Json = Map(
    "contract" -> contract.json,
    "parties" -> parties.map(kv => kv._1.toString -> Base58.encode(kv._2.pubKeyBytes).asJson ).asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCompletionCompanion

  override lazy val messageToSign: Array[Byte] = hashNoNonces

  override def toString: String = s"ContractCompletion(${json.noSpaces})"

}

object ContractCompletion {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: ContractCompletion): Try[Unit] = Try {
    require(tx.signatures.size == 3)
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.zip(tx.parties) forall { case (signature, (_, proposition)) =>
      signature.isValid(proposition, tx.messageToSign)
      MultiSignature25519(Set(signature)).isValid(tx.contractBox.proposition, tx.messageToSign)
    })
  }

}



abstract class TransferTransaction(val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                   val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                                   val signatures: IndexedSeq[Signature25519],
                                   override val fee: Long,
                                   override val timestamp: Long) extends BifrostTransaction {

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  def messageToSign0: Array[Byte] = (if(newBoxes.nonEmpty) newBoxes.map(_.bytes).reduce(_ ++ _) else Array[Byte]()) ++
    unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
}

trait TransferUtil {
  type Nonce = Long
  type Value = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def parametersForApply(from: IndexedSeq[(PrivateKey25519, Nonce)],
                         to: IndexedSeq[(PublicKey25519Proposition, Value)],
                         fee: Long,
                         timestamp: Long,
                         txType: String): (IndexedSeq[(PublicKey25519Proposition, Nonce)],
    IndexedSeq[Signature25519]) = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = txType match {
      case "PolyTransfer" => PolyTransfer(fromPub, to, fakeSigs, fee, timestamp)
      case "ArbitTransfer" => ArbitTransfer(fromPub, to, fakeSigs, fee, timestamp)
    }

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }
    (fromPub, sigs)
  }

  //noinspection ScalaStyle
  def parametersForCreate(w: BWallet, recipient: PublicKey25519Proposition, amount: Long, fee: Long, txType: String):
    (IndexedSeq[(PrivateKey25519, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {

    // Match only the type of boxes specified by txType
    val filteredBoxes: Seq[BifrostPublic25519NoncedBox] = txType match {
      case "PolyTransfer" => w.boxes().flatMap(
        a => if (a.box.isInstanceOf[PolyBox]) Some(a.box.asInstanceOf[BifrostPublic25519NoncedBox]) else None)
      case "ArbitTransfer" => w.boxes().flatMap(
        a => if (a.box.isInstanceOf[ArbitBox]) Some(a.box.asInstanceOf[BifrostPublic25519NoncedBox]) else None)
    }

    val from: IndexedSeq[(PrivateKey25519, Long, Long)] = filteredBoxes.flatMap {
      case b: BifrostPublic25519NoncedBox =>
        w.secretByPublicImage(b.proposition).map ((_, b.nonce, b.value))
    }.toIndexedSeq

    val canSend = from.map(_._3).sum
    val updatedBalance: (PublicKey25519Proposition, Long) = (w.publicKeys.find {
      case _: PublicKey25519Proposition => true
      case _ => false
    }.get.asInstanceOf[PublicKey25519Proposition], canSend - amount - fee)

    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(updatedBalance, (recipient, amount))

    require(from.map(_._3).sum - to.map(_._2).sum == fee)
    (from, to)
  }

  def validateTx(tx: TransferTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }
}

case class PolyTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                        override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                        override val signatures: IndexedSeq[Signature25519],
                        override val fee: Long,
                        override val timestamp: Long)
  extends TransferTransaction (from, to, signatures, fee, timestamp) {

  override type M = PolyTransfer

  override lazy val serializer = PolyTransferCompanion

  override def toString: String = s"PolyTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = PolyTransfer.nonceFromDigest(FastCryptographicHash("PolyTransfer".getBytes ++ prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      PolyBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "PolyTransfer".getBytes() ++ super.messageToSign0
}


object PolyTransfer extends TransferUtil {

  override type Nonce = Long
  override type Value = Long
  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): PolyTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "PolyTransfer")
    PolyTransfer(params._1, to, params._2, fee, timestamp)
  }

  //TODO seq of recipients and amounts
  def create(w: BWallet, recipient: PublicKey25519Proposition, amount: Long, fee: Long): Try[PolyTransfer] = Try {

    val params = parametersForCreate(w, recipient, amount, fee, "PolyTransfer")
    val timestamp = System.currentTimeMillis()
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp)
  }

  def validate(tx: PolyTransfer): Try[Unit] = Try {
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })
    validateTx(tx)
  }
}

case class ArbitTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                        override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                        override val signatures: IndexedSeq[Signature25519],
                        override val fee: Long,
                        override val timestamp: Long) extends TransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = ArbitTransfer

  override lazy val serializer = ArbitTransferCompanion

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = ArbitTransfer.nonceFromDigest(FastCryptographicHash("ArbitTransfer".getBytes ++ prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      ArbitBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes() ++ super.messageToSign0
}

object ArbitTransfer extends TransferUtil {

  override type Nonce = Long
  override type Value = Long
  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): ArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer")
    ArbitTransfer(params._1, to, params._2, fee, timestamp)
  }

  //TODO seq of recipients and amounts
  def create(w: BWallet, recipient: PublicKey25519Proposition, amount: Long, fee: Long): Try[ArbitTransfer] = Try {

    val params = parametersForCreate(w, recipient, amount, fee, "ArbitTransfer")
    val timestamp = System.currentTimeMillis()
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp)
  }

  def validate(tx: ArbitTransfer): Try[Unit] = Try {
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })
    validateTx(tx)
  }
}

case class ProfileTransaction(from: PublicKey25519Proposition,
                              signature: Signature25519,
                              keyValues: Map[String, String],
                              override val fee: Long,
                              override val timestamp: Long)
  extends BifrostTransaction{

  override type M = ProfileTransaction

  override lazy val serializer = ProfileTransactionCompanion

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq[Array[Byte]]()

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.map {
    boxId =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = signature
      }
  }

  override lazy val newBoxes: Traversable[BifrostBox] = keyValues.map {
    case (key, value) => ProfileBox(from, 0L, value, key)
  }

  override lazy val messageToSign: Array[Byte] = ProfileTransaction.messageToSign(timestamp, from, keyValues)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> Base58.encode(from.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "keyValues" -> keyValues.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
}

object ProfileTransaction {

  def messageToSign(timestamp: Long, from: PublicKey25519Proposition, keyValues: Map[String, String]): Array[Byte] = Bytes.concat(
    Longs.toByteArray(timestamp),
    from.pubKeyBytes,
    keyValues.asJson.toString().getBytes()
  )

  def validate(tx: ProfileTransaction): Try[Unit] = Try {
    // ensure no duplicates
    val keysSet = tx.keyValues.keys.toSet

    require(keysSet.subsetOf(ProfileBox.acceptableKeys))
    require(ProfileBox.acceptableRoleValues.contains(tx.keyValues("role")))
    require(tx.signature.isValid(tx.from, tx.messageToSign))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }
}