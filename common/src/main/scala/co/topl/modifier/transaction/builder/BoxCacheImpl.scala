package co.topl.modifier.transaction.builder

import cats.implicits._
import cats.Applicative
import co.topl.attestation.Address
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox}
import co.topl.modifier.{BoxReader, ProgramId}

class BoxCacheImpl[F[_]: Applicative](private val boxReader: BoxReader[ProgramId, Address]) extends BoxCache[F] {
  import BoxCache.BoxSet

  override def get(addresses: List[Address]): F[BoxCache.BoxSet] =
    addresses
      .flatMap(addr =>
        boxReader
          .getTokenBoxes(addr)
          .getOrElse(List())
          .map(addr -> _)
      )
      .foldLeft(BoxSet(List(), List(), List())) {
        case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = (addr -> box) :: boxes.polys)
        case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = (addr -> box) :: boxes.arbits)
        case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = (addr -> box) :: boxes.assets)
        case (boxes, _)                     => boxes
      }
      .pure[F]
}
