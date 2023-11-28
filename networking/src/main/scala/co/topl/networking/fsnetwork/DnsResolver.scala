package co.topl.networking.fsnetwork

import cats.Monad
import cats.data.OptionT
import cats.effect.Sync
import com.comcast.ip4s.{Dns, Hostname}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import cats.implicits._
import co.topl.networking.fsnetwork.DnsResolverInstances.DnsResolverSyntax
import co.topl.networking.p2p.RemoteAddress
import org.typelevel.log4cats.Logger

import java.time.Duration

trait DnsResolver[F[_]] {
  def resolving(host: String): F[Option[String]]
}

object DnsResolverInstances {

  class DefaultDnsResolver[F[_]: Dns: Sync] extends DnsResolver[F] {
    val dnsCacheSize: Int = 1000
    val expireAfterWriteDuration: Duration = java.time.Duration.ofMinutes(30)

    val cache: Cache[String, String] =
      Caffeine.newBuilder
        .maximumSize(dnsCacheSize)
        .expireAfterWrite(expireAfterWriteDuration)
        .build[String, String]()

    override def resolving(host: String): F[Option[String]] =
      if (cache.contains(host)) {
        Option(cache.getIfPresent(host)).pure[F]
      } else {
        doResolve(host).flatTap(hostname => hostname.traverse(rh => Sync[F].delay(cache.put(host, rh))))
      }

    private def doResolve(unresolvedHost: String): F[Option[String]] = {
      val resolver: Dns[F] = implicitly[Dns[F]]

      val res =
        for {
          host     <- OptionT.fromOption[F](Hostname.fromString(unresolvedHost))
          resolved <- OptionT(resolver.resolveOption(host))
        } yield resolved.toUriString
      res.value
    }
  }

  implicit class DnsResolverSyntax[F[_]](hostId: String)(implicit val resolver: DnsResolver[F]) {
    def resolving(): F[Option[String]] = resolver.resolving(hostId)
  }
}

trait DnsResolverHT[T, F[_]] {
  def resolving(host: T): F[Option[T]]
}

object DnsResolverHTInstances {

  implicit def remoteAddressResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[RemoteAddress, F] =
    (unresolvedHost: RemoteAddress) => {
      val resolver = implicitly[DnsResolver[F]]
      resolver.resolving(unresolvedHost.host).map(_.map(resolved => unresolvedHost.copy(host = resolved)))
    }

  implicit def peerToAddResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[KnownRemotePeer, F] =
    (unresolvedHost: KnownRemotePeer) => {
      val resolver = implicitly[DnsResolverHT[RemoteAddress, F]]
      resolver.resolving(unresolvedHost.address).map(_.map(resolved => unresolvedHost.copy(address = resolved)))
    }

  implicit def idAndPeerResolver[F[_]: Monad: DnsResolver: Logger]: DnsResolverHT[(HostId, Peer[F]), F] =
    (idPeer: (HostId, Peer[F])) =>
      idPeer._2.address.resolving().map(_.map(resolved => idPeer._1 -> idPeer._2.copy(address = resolved)))

  implicit def remotePeerResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[RemotePeer, F] =
    (unresolvedHost: RemotePeer) => {
      val resolver = implicitly[DnsResolverHT[RemoteAddress, F]]
      resolver.resolving(unresolvedHost.address).map(_.map(resolved => unresolvedHost.copy(address = resolved)))
    }

  implicit class DnsResolverHTSyntax[F[_], T](host: T)(implicit res: DnsResolverHT[T, F]) {

    def resolving(): F[Option[T]] = {
      val resolver: DnsResolverHT[T, F] = implicitly[DnsResolverHT[T, F]]
      resolver.resolving(host)
    }
  }
}
