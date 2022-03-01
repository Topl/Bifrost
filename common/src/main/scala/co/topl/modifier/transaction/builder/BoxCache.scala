package co.topl.modifier.transaction.builder

import cats.Applicative
import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.models.BoxReference
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox}
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.Int128

trait BoxCache[F[_]] {
  def get(addresses: List[Address]): F[BoxCache.BoxSet]
}

object BoxCache {

  final case class BoxSet(
    arbits: List[(Address, ArbitBox)],
    polys:  List[(Address, PolyBox)],
    assets: List[(Address, AssetBox)]
  )

  object BoxSet {
    sealed trait ToBoxReferencesFailure

    object ToBoxReferencesFailures {
      final case class InvalidAddress(address: Address) extends ToBoxReferencesFailure
      final case class InvalidQuantity(quantity: Int128) extends ToBoxReferencesFailure
    }

    def empty: BoxSet = BoxSet(Nil, Nil, Nil)

    def toBoxReferences(boxSet: BoxSet): Either[ToBoxReferencesFailure, List[BoxReference]] =
      for {
        polyReferences <-
          boxSet.polys.traverse(poly =>
            poly._1.toDionAddress
              .map(addr => addr -> poly._2.nonce)
              .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(poly._1): ToBoxReferencesFailure)
          )
        arbitReferences <-
          boxSet.arbits.traverse(arbit =>
            arbit._1.toDionAddress
              .map(addr => addr -> arbit._2.nonce)
              .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(arbit._1): ToBoxReferencesFailure)
          )
        assetReferences <-
          boxSet.assets.traverse(asset =>
            asset._1.toDionAddress
              .map(addr => addr -> asset._2.nonce)
              .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(asset._1): ToBoxReferencesFailure)
          )
      } yield polyReferences ++ arbitReferences ++ assetReferences
  }

  def apply[F[_]: Applicative](boxReader: BoxReader[ProgramId, Address]): BoxCache[F] = new BoxCacheImpl[F](boxReader)
}
