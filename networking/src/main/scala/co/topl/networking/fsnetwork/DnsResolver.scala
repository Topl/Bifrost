package co.topl.networking.fsnetwork

import cats.Monad
import cats.data.OptionT
import cats.effect.Sync
import com.comcast.ip4s.{Dns, Hostname}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import cats.implicits._
import co.topl.networking.p2p.RemoteAddress

import java.time.Duration

trait DnsResolver[F[_]] {
  def resolving(host: HostId): F[Option[HostId]]
}

object DnsResolverInstances {

  class DefaultDnsResolver[F[_]: Dns: Sync] extends DnsResolver[F] {
    val dnsCacheSize: Int = 1000
    val expireAfterWriteDuration: Duration = java.time.Duration.ofMinutes(30)

    val cache: Cache[HostId, HostId] =
      Caffeine.newBuilder
        .maximumSize(dnsCacheSize)
        .expireAfterWrite(expireAfterWriteDuration)
        .build[String, String]()

    override def resolving(host: HostId): F[Option[HostId]] =
      if (cache.contains(host)) {
        Option(cache.getIfPresent(host)).pure[F]
      } else {
        doResolve(host).flatTap(hostname => hostname.traverse(rh => Sync[F].delay(cache.put(host, rh))))
      }

    private def doResolve(unresolvedHost: HostId): F[Option[HostId]] = {
      val resolver: Dns[F] = implicitly[Dns[F]]

      val res =
        for {
          host     <- OptionT.fromOption[F](Hostname.fromString(unresolvedHost))
          resolved <- OptionT(resolver.resolveOption(host))
        } yield resolved.toUriString
      res.value
    }
  }

  implicit class DnsResolverSyntax[F[_]](hostId: HostId)(implicit val resolver: DnsResolver[F]) {
    def resolving(): F[Option[HostId]] = resolver.resolving(hostId)
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

  implicit def peerToAddResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[RemotePeer, F] =
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
