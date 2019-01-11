package bifrost.history

import java.util.UUID

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.utils.ScorexLogging
import scala.util.Try

class SRBStorage(val storage: LSMStore) extends ScorexLogging {

  def rollback(versionID: ByteArrayWrapper): Try[Unit] = Try { storage.rollback(versionID) }

  def checkpoint(versionID: ByteArrayWrapper): Try[Unit] = Try { storage.update(versionID, Seq(), Seq()) }

  def get(EntryID: ByteArrayWrapper): Option[ByteArrayWrapper] = storage.get(EntryID)

  def update(versionID: ByteArrayWrapper, changes: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Try[Unit] = Try { storage.update(versionID, Seq(), changes) }

}
