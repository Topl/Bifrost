package bifrost.srb

import java.io.File
import java.util.UUID

import bifrost.NodeViewModifier.ModifierId
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.forging.ForgingSettings
import bifrost.scorexMod.GenericMinimalState.VersionTag
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.Try

class SBR (store: LSMStore) extends ScorexLogging {

  def updateIfStateBoxTransaction(tx: BifrostTransaction) : Unit = {
    //    tx.newBoxes.foreach(b => if b.isInstanceOf[StateBox])
  }


  def updateWithSingleEntry(modifierId: ModifierId, k: UUID, v: Array[Byte]) : Try[SBR] = Try {
    val k_baw = StateBoxRegistry.uuid2baw(k)
    val v_baw = ByteArrayWrapper(v)
    store.update(ByteArrayWrapper(modifierId), Seq(), Seq((k_baw, v_baw)))
    SBR(store)
  }

  def get(k: UUID) : Option[Array[Byte]] =
    store.get(SBR.uuidToBaw(k))
      .map(_.data)


  def rollbackTo(version: VersionTag): Try[SBR] = Try {
    if (store.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rolling back SBR to: ${Base58.encode(version)}")
      store.rollback(ByteArrayWrapper(version))
      SBR(store)
    }
  }

}

object SBR extends ScorexLogging {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s: LSMStore) : SBR = {
    new SBR(s)
  }

  // UUID -> ByteArrayWrapper
  def uuidToBaw(v: UUID): ByteArrayWrapper = {
    ByteArrayWrapper(
      FastCryptographicHash(
        ByteArrayWrapper.fromLong(v.getMostSignificantBits).data ++
        ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data))
  }

  def readOrGenerate(settings: ForgingSettings): Option[SBR] = {
    val sbrDirOpt = settings.sbrDirOpt//.ensuring(_.isDefined, "sbr dir must be specified")
    val logDirOpt = settings.logDirOpt
    sbrDirOpt.map(readOrGenerate(_, logDirOpt, settings))
  }

  def readOrGenerate(sbrDir: String, logDirOpt: Option[String], settings: ForgingSettings): SBR = {
    val iFile = new File(s"$sbrDir")
    iFile.mkdirs()
    val sbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing sbr storage...")
        sbrStore.close()
      }
    })

    SBR(sbrStore)
  }

}

