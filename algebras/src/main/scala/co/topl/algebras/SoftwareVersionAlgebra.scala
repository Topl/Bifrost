package co.topl.algebras

/**
 * SoftwareVersionAlgebra
 *
 * @tparam F Effect type
 */
trait SoftwareVersionAlgebra[F[_]] {

  /**
   * Fetch current software version
   * @return software version
   */
  def fetchSoftwareVersion(): F[String]

  /**
   * Fetch latest software version
   *
   * @return latest software version
   */
  def fetchLatestSoftwareVersion(): F[String]

}
