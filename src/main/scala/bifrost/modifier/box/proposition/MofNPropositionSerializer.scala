package bifrost.modifier.box.proposition

import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints}

object MofNPropositionSerializer extends BifrostSerializer[MofNProposition] {

  override def serialize(obj: MofNProposition, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.m)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.setOfPubKeyBytes.size)
    obj.setOfPubKeyBytes.foreach(b => w.putBytes(b))
  }

  override def parse(r: Reader): MofNProposition = {
    val m: Int = r.getUInt().toIntExact
    val n: Int = r.getUInt().toIntExact
    val setOfPubKeyBytes: Set[Array[Byte]] = (0 until n).map(_ => r.getBytes(Constants25519.PubKeyLength)).toSet

    MofNProposition(m, setOfPubKeyBytes)
  }

  //  TODO: Jing - remove
  //
  //  override def toBytes(obj: MofNProposition): Array[Byte] = Bytes.concat(
  //    Ints.toByteArray(obj.m),
  //    Ints.toByteArray(obj.setOfPubKeyBytes.size),
  //    obj.setOfPubKeyBytes.toList.sortBy(Base58.encode).foldLeft(Array[Byte]())((a: Array[Byte],
  //                                                                               b: Array[Byte]) => a ++ b)
  //  )
  //
  //  override def parseBytes(bytes: Array[Byte]): Try[MofNProposition] = Try {
  //
  //    val m = Ints.fromByteArray(bytes.take(Ints.BYTES))
  //    val n = Ints.fromByteArray(bytes.slice(Ints.BYTES, 2 * Ints.BYTES))
  //
  //    val setPubKeys = (0 until n).map { i =>
  //      bytes.slice(2 * Ints.BYTES + i * Constants25519.PubKeyLength,
  //                  2 * Ints.BYTES + (i + 1) * Constants25519.PubKeyLength)
  //    }.foldLeft(Set[Array[Byte]]())((set: Set[Array[Byte]], pubKey: Array[Byte]) => set + pubKey)
  //
  //    MofNProposition(m, setPubKeys)
  //  }
}
