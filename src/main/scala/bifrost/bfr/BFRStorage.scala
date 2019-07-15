//package bifrost.bfr
//
//import bifrost.utils.ScorexLogging
//import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
//
//import scala.util.Try
//
//class BFRStorage(val storage: LSMStore) extends ScorexLogging  {
//
//    def rollback(versionID: ByteArrayWrapper): Try[Unit] = Try { storage.rollback(versionID) }
//
//    def checkpoint(versionID: ByteArrayWrapper): Try[Unit] = Try { storage.update(versionID, Seq(), Seq()) }
//
//    def get(EntryID: ByteArrayWrapper): Option[ByteArrayWrapper] = { storage.get(EntryID)}
//
//    def update(versionID: ByteArrayWrapper, changes: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Try[Unit] = Try { storage.update(versionID, Seq(), changes) }
//
//
//}
