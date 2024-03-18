package co.topl.algebras

trait Telemetry {

  implicit class PeersHandler[F[_]: Async: Logger](
    peers:         Map[HostId, Peer[F]],
    networkConfig: P2PNetworkConfig
  ) {
    def generateMetrics: Map[HostId, Peer[F]] = {}
  }
}
