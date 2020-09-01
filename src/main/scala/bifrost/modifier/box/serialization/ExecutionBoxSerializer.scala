package bifrost.modifier.box.serialization

import java.util.UUID

import bifrost.modifier.box.{ExecutionBox, ProgramBox}
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object ExecutionBoxSerializer extends BifrostSerializer[ExecutionBox] {

  override def serialize(obj: ExecutionBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* stateBoxUUIDs: Seq[UUID], List of uuids of state boxes from ProgramBoxRegistry */
    w.putUInt(obj.stateBoxUUIDs.length)
    obj.stateBoxUUIDs.foreach { id =>
      w.putLong(id.getMostSignificantBits)
      w.putLong(id.getLeastSignificantBits)
    }

    /* codeBoxIds: Seq[Array[Byte]] */
    w.putUInt(obj.codeBoxIds.length)
    obj.codeBoxIds.foreach{id =>
      w.putUInt(id.length)
      w.putBytes(id)
    }
  }

  override def parse(r: Reader): ExecutionBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    /* stateBoxUUIDs: Seq[UUID], List of uuids of state boxes from ProgramBoxRegistry */
    val stateBoxUUIDsLength: Int = r.getUInt().toIntExact
    val stateBoxUUIDs: Seq[UUID] = (0 until stateBoxUUIDsLength).map(_ => new UUID(r.getLong(), r.getLong()))

    /* codeBoxIds: Seq[Array[Byte]] */
    val codeBoxIdsLength: Int = r.getUInt().toIntExact

    val codeBoxIds: Seq[Array[Byte]] = (0 until codeBoxIdsLength).map{_ =>
      val idLength: Int = r.getUInt().toIntExact
      r.getBytes(idLength)
    }

    ExecutionBox(programBox.proposition, programBox.nonce, programBox.value, stateBoxUUIDs, codeBoxIds)
  }

  // TODO: Jing - remove
  //
  //  def toBytes(obj: ExecutionBox): Array[Byte] = {
  //
  //    val boxType = "ExecutionBox"
  //    Bytes.concat(
  //      Ints.toByteArray(boxType.getBytes.length),
  //      boxType.getBytes,
  //      obj.proposition.pubKeyBytes,
  //      Longs.toByteArray(obj.nonce),
  //      Longs.toByteArray(obj.value.getMostSignificantBits),
  //      Longs.toByteArray(obj.value.getLeastSignificantBits),
  //      Ints.toByteArray(obj.stateBoxUUIDs.length),
  //      obj.stateBoxUUIDs.foldLeft(Array[Byte]()) {
  //        (arr, x) =>
  //          arr ++ Bytes.concat(
  //            Longs.toByteArray(x.getMostSignificantBits),
  //            Longs.toByteArray(x.getLeastSignificantBits)
  //          )
  //      },
  //      Ints.toByteArray(obj.codeBoxIds.length),
  //      obj.codeBoxIds.foldLeft(Array[Byte]()) {
  //        (arr, x) => arr ++ x
  //      }
  //    )
  //  }
  //
  //  def parseBytes(obj: Array[Byte]): Try[ExecutionBox] = Try {
  //    var takenBytes = 0
  //
  //    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
  //    takenBytes += Ints.BYTES
  //
  //    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
  //    takenBytes += boxTypeLength
  //
  //    require(boxType == "ExecutionBox")
  //
  //    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
  //    takenBytes += Constants25519.PubKeyLength
  //
  //    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
  //    takenBytes += Longs.BYTES
  //
  //    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
  //      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2 * Longs.BYTES)))
  //    takenBytes += Longs.BYTES * 2
  //
  //    val stateBoxUUIDsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
  //    takenBytes += Ints.BYTES
  //
  //    var stateBoxUUIDs = Seq[UUID]()
  //    for (_ <- 1 to stateBoxUUIDsLength) {
  //      val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
  //        Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + Longs.BYTES * 2)))
  //      takenBytes += Longs.BYTES * 2
  //      stateBoxUUIDs = stateBoxUUIDs :+ uuid
  //    }
  //
  //    val codeBoxIdsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
  //    takenBytes += Ints.BYTES
  //
  //    val codeBoxIds: Seq[Array[Byte]] = (0 until codeBoxIdsLength).map { i =>
  //      val id: Array[Byte] = obj.slice(takenBytes + i * (4 * Longs.BYTES), takenBytes + (i + 1) * (4 * Longs.BYTES))
  //      id
  //    }
  //    takenBytes += Longs.BYTES * 4 * codeBoxIdsLength
  //
  //    ExecutionBox(prop, nonce, uuid, stateBoxUUIDs, codeBoxIds)
  //  }
}
