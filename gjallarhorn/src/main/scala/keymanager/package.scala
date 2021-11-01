package object keymanager {

  /** This variable keeps track of the current network */
  private var _networkPrefix: NetworkPrefix = 48.toByte

  // setters
  private[keymanager] def networkPrefix_=(value: NetworkPrefix): Unit = _networkPrefix = value

  // getters
  def networkPrefix: NetworkPrefix = _networkPrefix
}
