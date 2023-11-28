package co.topl.networking.fsnetwork

import cats.{Applicative, Monad}
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.networking.p2p.RemoteAddress
import com.comcast.ip4s.{Dns, IpAddress}
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait ReverseDnsResolver[F[_]] {
  def reverseResolving(host: HostId): F[HostId]
}

object ReverseDnsResolverInstances {

  class NoOpReverseResolver[F[_]: Applicative] extends ReverseDnsResolver[F] {
    override def reverseResolving(host: HostId): F[HostId] = host.pure[F]
  }

  class DefaultReverseDnsResolver[F[_]: Dns: Sync] extends ReverseDnsResolver[F] {
    private val reverseDnsCacheSize: Int = 1000
    val expireAfterWriteDuration: FiniteDuration = 30.minutes

    val cache: CaffeineCache[F, String, String] =
      CaffeineCache[F, String, String](
        Caffeine.newBuilder
          .maximumSize(reverseDnsCacheSize)
          .build[String, Entry[String]]()
      )

    override def reverseResolving(hostIdAsIp: HostId): F[HostId] =
      cache.cachingF(hostIdAsIp)(expireAfterWriteDuration.some)(doResolve(hostIdAsIp))

    private def doResolve(hostIdAsIp: HostId): F[HostId] = {
      val resolver: Dns[F] = implicitly[Dns[F]]
      val res =
        for {
          ip       <- OptionT.fromOption[F](IpAddress.fromString(hostIdAsIp))
          resolved <- OptionT(resolver.reverseOption(ip))
        } yield resolved.normalized.toString
      // if we failed to get hostname then still use ip
      res.value.map(_.getOrElse(hostIdAsIp))
    }
  }

  implicit class ReverseDnsResolverSyntax[F[_]](hostId: HostId)(implicit val resolver: ReverseDnsResolver[F]) {
    def reverseResolving(): F[HostId] = resolver.reverseResolving(hostId)
  }
}

trait ReverseDnsResolverHT[T, F[_]] {
  def reverseResolving(host: T): F[T]
}

object ReverseDnsResolverHTInstances {

  implicit def reverseRemoteAddressResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemoteAddress, F] =
    (resolvedHost: RemoteAddress) => {
      val resolver = implicitly[ReverseDnsResolver[F]]
      resolver.reverseResolving(resolvedHost.host).map(resolved => resolvedHost.copy(host = resolved))
    }

  implicit def reversePeerToAddResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemotePeer, F] =
    (resolvedHost: RemotePeer) => {
      val resolver = implicitly[ReverseDnsResolverHT[RemoteAddress, F]]
      resolver.reverseResolving(resolvedHost.address).map(resolved => resolvedHost.copy(address = resolved))
    }

  implicit class ReverseDnsResolverHTSyntax[F[_], T](host: T)(implicit res: ReverseDnsResolverHT[T, F]) {

    def reverseResolving(): F[T] = {
      val resolver: ReverseDnsResolverHT[T, F] = implicitly[ReverseDnsResolverHT[T, F]]
      resolver.reverseResolving(host)
    }
  }
}
