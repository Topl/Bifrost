import attestation.AddressEncoder.NetworkPrefix

package object keymanager {
  //default value:
  private var _networkPrefix: NetworkPrefix = 48.toByte

  //setters
  private[keymanager] def networkPrefix_= (value: NetworkPrefix): Unit = _networkPrefix = value

  //getters
  def networkPrefix: NetworkPrefix = _networkPrefix
}
