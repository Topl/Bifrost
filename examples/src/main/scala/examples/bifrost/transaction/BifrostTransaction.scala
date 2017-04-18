package examples.bifrost.transaction

import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.transaction.StableCoinTransfer.Nonce
import examples.bifrost.contract._
import examples.bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import examples.bifrost.transaction.box.{BifrostBox, ContractBox, PublicKey25519NoncedBox, StableCoinBox}
import examples.bifrost.transaction.proof.MultiSignature25519
import examples.hybrid.wallet.HWallet
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait BifrostTransaction extends GenericBoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, BifrostBox]

sealed abstract class ContractTransaction extends BifrostTransaction

// 3 signatures FOR A SPECIFIC MESSAGE <agreement: Agreement>
// ContractCreation(agreement ++ nonce, IndexSeq(pk1, pk2, pk3), IndexSeq(sign1(agreement ++ nonce), sign2(agreement ++ nonce), sign3(agreement ++ nonce)) )
// validity check: decrypt[pk1] sign1(agreement) === agreement
// agreement specifies "executeBy" date
case class ContractCreation(agreement: Agreement,
                            parties: IndexedSeq[PublicKey25519Proposition],
                            signatures: IndexedSeq[Signature25519],
                            fee: Long,
                            timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

  lazy val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)

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
      parties.foldLeft(Array[Byte]())((a,b) => a ++ b.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )


  override lazy val newBoxes: Traversable[BifrostBox] = {
    val nonce = ContractCreation.nonceFromDigest(FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces))
    val agreementString = new String(AgreementCompanion.toBytes(agreement))
    IndexedSeq(ContractBox(proposition, nonce, agreementString))
  }

  override lazy val json: Json = Map(
    "agreement" -> agreement.json,
    "parties" -> parties.map( p => Base58.encode(p.pubKeyBytes).asJson ).asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"ContractCreation(${json.noSpaces})"

}

object ContractCreation {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def validate(tx: ContractCreation): Try[Unit] = Try {

    Agreement.validate(tx.agreement)

    require(tx.parties.size == tx.signatures.size)
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.zip(tx.parties) forall { case (signature, proposition) =>
      signature.isValid(proposition, tx.agreement.toString.getBytes)
    })
  }

}


trait TransferTransaction extends BifrostTransaction

case class StableCoinTransfer(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                              to: IndexedSeq[(PublicKey25519Proposition, Long)],
                              signatures: IndexedSeq[Signature25519],
                              override val fee: Long,
                              override val timestamp: Long)
  extends TransferTransaction {

  override type M = StableCoinTransfer

  override lazy val serializer = StableCoinTransferCompanion

  override def toString: String = s"TransferTransaction(${json.noSpaces})"

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

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = StableCoinTransfer.nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      StableCoinBox(prop, nonce, value)
  }

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
}

object StableCoinTransfer {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): StableCoinTransfer = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = StableCoinTransfer(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new StableCoinTransfer(fromPub, to, sigs, fee, timestamp)
  }

  //TODO seq of recipients and amounts
  def create(w: HWallet, recipient: PublicKey25519Proposition, amount: Long, fee: Long): Try[StableCoinTransfer] = Try {

    val from: IndexedSeq[(PrivateKey25519, Long, Long)] = w.boxes().flatMap { b =>
      w.secretByPublicImage(b.box.proposition).map(s => (s, b.box.nonce, b.box.value))
    }.toIndexedSeq
    val canSend = from.map(_._3).sum
    val charge: (PublicKey25519Proposition, Long) = (w.publicKeys.head, canSend - amount - fee)

    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(charge, (recipient, amount))

    require(from.map(_._3).sum - to.map(_._2).sum == fee)

    val timestamp = System.currentTimeMillis()
    StableCoinTransfer(from.map(t => t._1 -> t._2), to, fee, timestamp)
  }

  def validate(tx: StableCoinTransfer): Try[Unit] = Try {
      require(tx.from.size == tx.signatures.size)
      require(tx.to.forall(_._2 >= 0))
      require(tx.fee >= 0)
      require(tx.timestamp >= 0)
      require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
        proof.isValid(prop, tx.messageToSign)
      })
  }

}