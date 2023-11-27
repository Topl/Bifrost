package co.topl.algebras

/**
 * AdminRpc
 * An interaction layer intended to convey the admin endpoints of a blockchain node and its services.
 *
 * @tparam F Effect type
 * @tparam S Response container, Ex: Stream, Seq, etc.
 */
trait AdminRpc[F[_], S[_]] {

  def fetchSoftwareVersion(): F[String]

}
