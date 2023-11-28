package co.topl.networking.fsnetwork

import cats.Show
import cats.implicits.showInterpolator
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.models.{BlockHeaderToBodyValidationFailure, BlockHeaderValidationFailure}
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError, BodyValidationError}
import co.topl.networking.fsnetwork.NetworkQualityError._
import co.topl.networking.fsnetwork.PeersManager.Message.PingPongMessagePing
import co.topl.node.models.{CurrentKnownHostsReq, PingMessage}
import co.topl.typeclasses.implicits._

trait P2PShowInstances {

  implicit val showTransactionSyntaxError: Show[TransactionSyntaxError] =
    Show.fromToString

  implicit val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  implicit val showBodySyntaxError: Show[BodySyntaxError] =
    Show.fromToString

  implicit val showBodySemanticError: Show[BodySemanticError] =
    Show.fromToString

  implicit val showBodyAuthorizationError: Show[BodyAuthorizationError] =
    Show.fromToString

  implicit val showHeaderToBodyError: Show[BlockHeaderToBodyValidationFailure] =
    Show.fromToString

  implicit val showBodyValidationError: Show[BodyValidationError] =
    Show.fromToString

  implicit val showKnownHostReq: Show[CurrentKnownHostsReq] = req => s"CurrentKnownHostsReq(maxCount=${req.maxCount})"

  implicit val showPingMessage: Show[PingMessage] = message => s"PingMessage with len ${message.ping.length}"

  implicit val showNoPongMessage: Show[NetworkQualityError] = {
    case _: NoPongMessage.type        => "Failed to receive pong message"
    case _: IncorrectPongMessage.type => "Receive incorrect pong message"
  }

  implicit val showPongMessage: Show[PingPongMessagePing] = {
    case PingPongMessagePing(host, Right(delay))       => show"Received pong delay $delay from host $host"
    case PingPongMessagePing(host, Left(networkError)) => show"$networkError from host $host"
  }

  implicit val showPeerState: Show[PeerState] = {
    case PeerState.Banned  => "BANNED"
    case PeerState.Cold    => "COLD"
    case PeerState.Warm    => "WARM"
    case PeerState.Hot     => "HOT"
    case PeerState.Unknown => "UNKNOWN"
  }

  implicit val showUnverifiedHeader: Show[UnverifiedBlockHeader] = header =>
    show"UnverifiedBlockHeader(id=${header.blockHeader.id}, " +
    show"source=${header.source}, downloadTime=${header.downloadTimeMs} ms)"

  implicit val showNetworkProperties: Show[NetworkProperties] = prop => plainClassAsString(prop)

  implicit val showNetworkConfig: Show[P2PNetworkConfig] = config =>
    show"Network properties: ${config.networkProperties};" ++
    s" Slot duration: ${config.slotDuration.toMillis} ms;" ++
    s" BlockNoveltyInitialValue: ${config.blockNoveltyInitialValue};" ++
    s" BlockNoveltyReputationStep: ${config.blockNoveltyReputationStep};" ++
    s" BlockNoveltyDecoy: ${config.blockNoveltyDecoy};" ++
    s" PerformanceReputationIdealValue: ${config.performanceReputationIdealValue}" ++
    s" PerformanceReputationMaxDelay: ${config.performanceReputationMaxDelay} ms" ++
    s" RemotePeerNoveltyInSlots: ${config.remotePeerNoveltyInSlots}" ++
    s" WarmHostsUpdateInterval: ${config.warmHostsUpdateInterval.toMillis} ms" ++
    s" AggressiveP2PRequestInterval: ${config.aggressiveP2PRequestInterval.toMillis} ms"

  implicit def showPeer[F[_]]: Show[Peer[F]] = { peer: Peer[F] =>
    "<<< Peer" +
    s" ${peer.ip}:[${peer.remoteServerPort.map(_.toString).getOrElse("")}];" +
    s" State is ${peer.state.toString};" +
    s" Actor is ${if (peer.actorOpt.isDefined) "present" else "absent"};" +
    s" Remote peer is ${if (peer.remoteNetworkLevel) "active" else "no active"};" +
    f" Rep: block=${peer.blockRep}%.2f, perf=${peer.perfRep}%.2f, new=${peer.newRep}, mean=${peer.reputation}%.2f;" +
    s" With total ${peer.closedTimestamps.size} closes with timestamps ${peer.closedTimestamps}" +
    s" >>>"
  }

  implicit val remotePeerShow: Show[RemotePeer] = { remotePeer: RemotePeer =>
    s"Remote peer: ${remotePeer.address}"
  }

  // TODO work with multi level classes
  private def plainClassAsString(objToString: Product): String =
    (objToString.productElementNames zip objToString.productIterator)
      .map { case (paramName, param) => s"$paramName=$param, " }
      .foldLeft("")(_ + _)
}

object P2PShowInstances extends P2PShowInstances
