package co.topl.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, LogStore}

trait StoreInterface {

  protected val storage: LogStore

  /** method to retrieve data from storage */
  protected def getFromStorage(id: Array[Byte]): Option[Array[Byte]] =
    storage.get(ByteArrayWrapper(id)).map(_.data)

  /** method to close storage on termination */
  def closeStorage(): Unit = storage.close()

}
