package co.topl.networking

trait StandardPeer[F[_]] {

  def connectionHealthProtocol: F[TypedProtocolInitializer[F]]
  def chainSyncProtocol: F[TypedProtocolInitializer[F]]
  def blockFetchProtocol: F[TypedProtocolInitializer[F]]

}
