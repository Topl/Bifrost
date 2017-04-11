package examples.bifrost.transaction

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import scorex.core.serialization.Serializer
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.signatures.Curve25519

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
case class BifrostBox[T](override val proposition: PublicKey25519Proposition, override val nonce: Long, override val value: T)
  extends PublicKeyNoncedBox[PublicKey25519Proposition] {

  override type M = PublicKey25519NoncedBox

  override def serializer: Serializer[BifrostBox[T]] = BifrostBoxSerializer[T]

}


object BifrostBoxSerializer extends Serializer[BifrostBox[Any]] {

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

  override def toBytes(obj: BifrostBox[Any]): Array[Byte] = {
    val serialised = serialise(obj.value)

    obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Ints.toByteArray(serialised.length) ++ serialised
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostBox[Any]] = Try {
    val pk = PublicKey25519Proposition(bytes.take(Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(Constants25519.PubKeyLength, Constants25519.PubKeyLength + Longs.BYTES))

    val numReadBytes = Constants25519.PubKeyLength + Longs.BYTES

    val valueLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val value = deserialise(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + valueLength))
    BifrostBox(pk, nonce, value)
  }
}