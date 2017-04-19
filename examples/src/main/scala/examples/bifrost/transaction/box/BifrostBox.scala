package examples.bifrost.transaction.box

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.{Constants25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class BifrostBox(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                      nonce: Long,
                      value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = BifrostBox

  override def serializer: Serializer[BifrostBox] = BifrostBoxSerializer

  // lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: BifrostBox => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}


object BifrostBoxSerializer extends Serializer[BifrostBox] {

  def serialise(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close
    stream.toByteArray
  }

  def deserialise(bytes: Array[Byte]): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value
  }

  override def toBytes(obj: BifrostBox): Array[Byte] = obj match {
    case bp: StableCoinBox => (new StableCoinBoxSerializer).toBytes(obj.asInstanceOf[StableCoinBox])
    case c: ContractBox => (new ContractBoxSerializer).toBytes(obj.asInstanceOf[ContractBox])
    case _ => throw new Exception("Unanticipated BifrostBox type")
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostBox] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    typeStr match {
      case "StableCoinBox" => (new StableCoinBoxSerializer).parseBytes(bytes)
      case "ContractBox" => (new ContractBoxSerializer).parseBytes(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}

case class StableCoinBox(proposition: PublicKey25519Proposition,
                         nonce: Long,
                         value: Long) extends BifrostBox(proposition, nonce, value) {
  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)
}

class StableCoinBoxSerializer extends Serializer[StableCoinBox] {

  def toBytes(obj: StableCoinBox): Array[Byte] = {

    val boxType = "StableCoinBox"

    Ints.toByteArray(boxType.getBytes.length) ++ boxType.getBytes ++ obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Longs.toByteArray(obj.value)
  }

  override def parseBytes(bytes: Array[Byte]): Try[StableCoinBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    val numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength, numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))

    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES

    val value = Longs.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Longs.BYTES))
    StableCoinBox(pk, nonce, value)
  }

}


case class ContractBox(proposition: MofNProposition, nonce: Long, value: String ) extends BifrostBox(proposition, nonce, value) {

  lazy val id: Array[Byte] = FastCryptographicHash(
    MofNPropositionSerializer.toBytes(proposition) ++
    Longs.toByteArray(nonce) ++
    value.getBytes
  )

}

class ContractBoxSerializer extends Serializer[ContractBox] {

  def toBytes(obj: ContractBox): Array[Byte] = {

    val boxType = "ContractBox"

    Ints.toByteArray(boxType.getBytes.length) ++
      boxType.getBytes ++
      MofNPropositionSerializer.toBytes(obj.proposition) ++
      Longs.toByteArray(obj.nonce) ++
      Ints.toByteArray(obj.value.getBytes.length) ++
      obj.value.getBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    var numReadBytes = Ints.BYTES + typeLen

    val numOfPk = Ints.fromByteArray(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + 2*Ints.BYTES))
    val endIndex = numReadBytes + 2*Ints.BYTES + numOfPk*Constants25519.PubKeyLength
    val proposition = MofNPropositionSerializer.parseBytes(bytes.slice(numReadBytes, endIndex)).get
    numReadBytes = endIndex

    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))

    numReadBytes += Longs.BYTES

    val valueLen = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))

    val value = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + valueLen))

    ContractBox(proposition, nonce, value)
  }

}