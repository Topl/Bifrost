package co.topl.nodeView.state

import co.topl.nodeView.KeyValueStore

trait StoreInterface extends AutoCloseable {

  protected val storage: KeyValueStore

  /** method to retrieve data from storage */
  protected def getFromStorage(id: Array[Byte]): Option[Array[Byte]] =
    storage.get(id)

  /** method to close storage on termination */
  override def close(): Unit =
    storage.close()

}
