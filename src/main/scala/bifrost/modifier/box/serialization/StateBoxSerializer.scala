package bifrost.modifier.box.serialization

import bifrost.modifier.box.{ProgramBox, StateBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.{Json, parser}

object StateBoxSerializer extends BifrostSerializer[StateBox] {

  override def serialize(obj: StateBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* state: Json, JSON representation of JS Variable Declarations */
    w.putIntString(obj.state.noSpaces)
  }

  override def parse(r: Reader): StateBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    val state: Json = parser.parse(r.getIntString()) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    StateBox(programBox.proposition, programBox.nonce, programBox.value, state)
  }

  // TODO: Jing - remove
  //
  //  override def toBytes(obj: StateBox): Array[Byte] = {
  //    val boxType = "StateBox"
  //    Bytes.concat(
  //      Ints.toByteArray(boxType.getBytes.length),
  //      boxType.getBytes,
  //      Longs.toByteArray(obj.nonce),
  //      Longs.toByteArray(obj.value.getMostSignificantBits),
  //      Longs.toByteArray(obj.value.getLeastSignificantBits),
  //      Ints.toByteArray(obj.state.noSpaces.getBytes.length),
  //      obj.state.noSpaces.getBytes,
  //      obj.proposition.pubKeyBytes
  //    )
  //  }
  //
  //  override def parseBytes(obj: Array[Byte]): Try[StateBox] = Try {
  //    var takenBytes = 0
  //
  //    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
  //    takenBytes += Ints.BYTES
  //
  //    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
  //    takenBytes += boxTypeLength
  //
  //    require(boxType == "StateBox")
  //
  //    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
  //    takenBytes += Longs.BYTES
  //
  //    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
  //      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2*Longs.BYTES)))
  //    takenBytes += Longs.BYTES*2
  //
  //    val stateLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
  //    takenBytes += Ints.BYTES
  //
  //    val state: Json = parser.parse(new String(obj.slice(takenBytes, takenBytes + stateLength))) match {
  //      case Left(f) => throw f
  //      case Right(j: Json) => j
  //    }
  //    takenBytes += stateLength
  //
  //    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
  //    takenBytes += Constants25519.PubKeyLength
  //
  //    StateBox(prop, nonce, uuid, state)
  //  }
}
