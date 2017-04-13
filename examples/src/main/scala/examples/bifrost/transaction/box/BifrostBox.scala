package examples.bifrost.transaction.box

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.google.common.primitives.{Ints, Longs}
import scorex.core.serialization.Serializer
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class BifrostBox(proposition: PublicKey25519Proposition,
                      nonce: Long,
                      value: Any) extends GenericBox[PublicKey25519Proposition, Any] {

  override type M = BifrostBox

  override def serializer: Serializer[BifrostBox] = BifrostBoxSerializer

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

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
    case bp: BifrostPaymentBox => (new BifrostPaymentBoxSerializer).toBytes(obj.asInstanceOf[BifrostPaymentBox])
    case c: ContractBox => (new ContractBoxSerializer).toBytes(obj.asInstanceOf[ContractBox])
    case _ => throw new Exception("Unanticipated BifrostBox type")
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostBox] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    typeStr match {
      case "BifrostPaymentBox" => (new BifrostPaymentBoxSerializer).parseBytes(bytes)
      case "ContractBox" => (new ContractBoxSerializer).parseBytes(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}

case class BifrostPaymentBox(proposition: PublicKey25519Proposition,
                             nonce: Long,
                             value: Long) extends BifrostBox(proposition, nonce, value) {



}

class BifrostPaymentBoxSerializer extends Serializer[BifrostPaymentBox] {

  def toBytes(obj: BifrostPaymentBox): Array[Byte] = {

    val boxType = "BifrostPaymentBox"

    Ints.toByteArray(boxType.getBytes.length) ++ boxType.getBytes ++ obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Longs.toByteArray(obj.value)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostPaymentBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    val numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength, numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))

    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES

    val value = Longs.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Longs.BYTES))
    BifrostPaymentBox(pk, nonce, value)
  }

}


case class ContractBox(proposition: PublicKey25519Proposition,
                             nonce: Long,
                             value: String) extends BifrostBox(proposition, nonce, value) {



}

class ContractBoxSerializer extends Serializer[ContractBox] {

  def toBytes(obj: ContractBox): Array[Byte] = {

    val boxType = "ContractBox"

    Ints.toByteArray(boxType.getBytes.length) ++
      boxType.getBytes ++
      obj.proposition.pubKeyBytes ++
      Longs.toByteArray(obj.nonce) ++
      Ints.toByteArray(obj.value.getBytes.length) ++
      obj.value.getBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractBox] = Try {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))

    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    val numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength, numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))

    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES

    val valueLen = Ints.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Ints.BYTES))

    val value = new String(bytes.slice(curReadBytes + Ints.BYTES, curReadBytes + Ints.BYTES + valueLen))
    ContractBox(pk, nonce, value)
  }

}