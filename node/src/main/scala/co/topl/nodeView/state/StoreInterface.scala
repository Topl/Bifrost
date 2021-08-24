package co.topl.nodeView.state

import co.topl.db.VersionedKVStore

trait StoreInterface {

  protected val storage: VersionedKVStore

  /** method to retrieve data from storage */
  protected def getFromStorage(id: Array[Byte]): Option[Array[Byte]] =
    storage.get(id)

  /** method to close storage on termination */
  def closeStorage(): Unit = storage.close()

}
