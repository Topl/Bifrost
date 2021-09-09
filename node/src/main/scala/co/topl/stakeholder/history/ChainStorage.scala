package co.topl.stakeholder.history

import java.io.{BufferedWriter, File, FileWriter}

import co.topl.stakeholder.components.{Serializer, Tine}
import co.topl.stakeholder.primitives.{ByteStream, SimpleTypes}
import co.topl.stakeholder.primitives.Base58

import scala.util.{Failure, Success, Try}

/**
 * AMS 2020:
 * Chain is stored to disk with each reorg so the node will have quick access to vrf nonces and slotIds
 * Local chain is restored from the last saved configuration upon restart
 */

class ChainStorage(dir: String) extends SimpleTypes {
  import co.topl.stakeholder.components.Serializer.DeserializeChain

  def readFile(file: File): String = {
    val bufferedSource = scala.io.Source.fromFile(file)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines.head
  }

  def restore(cid: Hash, serializer: Serializer)(implicit blocks: BlockStorage): Option[Tine] =
    Try {
      val file = new File(s"$dir/history/chain/${Base58.encode(cid.data)}")
      println(s"Local Chain file exists: ${file.exists()}")
      val chainTxt: String = readFile(file)
      val cBytes: Array[Byte] = Base58.decode(chainTxt).get
      serializer.fromBytes(new ByteStream(cBytes, DeserializeChain)) match {
        case data: TineData @unchecked =>
          println("Recovered Local Chain")
          Tine(data)
        case _ =>
          println("Error: tine not loaded")
          new Tine
      }
    } match {
      case Success(value) => Some(value)
      case Failure(_) =>
        None
    }

  def store(chain: Tine, cid: Hash, serializer: Serializer): Unit = {
    val cBytes = serializer.getBytes(chain)
    val file = new File(s"$dir/history/chain/${Base58.encode(cid.data)}")
    file.getParentFile.mkdirs
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Base58.encode(cBytes))
    bw.close()
  }

}
