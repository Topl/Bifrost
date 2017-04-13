package examples.bifrost.transaction

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.GenericBox
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.signatures.Curve25519

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
case class BifrostBox[T](proposition: PublicKey25519Proposition,
                         nonce: Long,
                         value: T)(subclassDeser: Serializer[BifrostBox[T]]) extends GenericBox[PublicKey25519Proposition, T] {

  override type M = BifrostBox[T]

  override def serializer: Serializer[BifrostBox[T]] = subclassDeser

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: BifrostBox[proposition] => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object BifrostBox {

  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
}


class BifrostBoxSerializer[T] extends Serializer[BifrostBox[T]] {

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

  override def toBytes(obj: BifrostBox[T]): Array[Byte] = {
    val serialised = serialise(obj.value)

    obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Ints.toByteArray(serialised.length) ++ serialised
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostBox[T]] = Try {
    val pk = PublicKey25519Proposition(bytes.take(Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(Constants25519.PubKeyLength, Constants25519.PubKeyLength + Longs.BYTES))

    val numReadBytes = Constants25519.PubKeyLength + Longs.BYTES

    val valueLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val value: T = deserialise(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + valueLength)).asInstanceOf[T]
    BifrostBox(pk, nonce, value)(this)
  }
}