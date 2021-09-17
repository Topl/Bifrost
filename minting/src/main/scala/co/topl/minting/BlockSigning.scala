package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.algebras.{BlockSigningAlgebra, ClockAlgebra, KeyEvolverAlgebra}
import co.topl.crypto.typeclasses.implicits._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2}
import co.topl.typeclasses.implicits._

object BlockSigning {

  object Eval {

    def make[F[_]: Monad](clockAlgebra: ClockAlgebra[F], evolver: KeyEvolverAlgebra[F]): BlockSigningAlgebra[F] =
      unsignedBlock =>
        evolver
          .evolvedKey(unsignedBlock.unsignedHeader.slot)
          .map(evolvedKey => evolvedKey.certify(unsignedBlock.unsignedHeader))
          .map(kesCertificate =>
            BlockHeaderV2(
              unsignedBlock.unsignedHeader.parentHeaderId,
              unsignedBlock.unsignedHeader.parentSlot,
              unsignedBlock.unsignedHeader.txRoot,
              unsignedBlock.unsignedHeader.bloomFilter,
              unsignedBlock.unsignedHeader.timestamp,
              unsignedBlock.unsignedHeader.height,
              unsignedBlock.unsignedHeader.slot,
              unsignedBlock.unsignedHeader.vrfCertificate,
              kesCertificate,
              unsignedBlock.unsignedHeader.thresholdEvidence,
              unsignedBlock.unsignedHeader.metadata,
              unsignedBlock.unsignedHeader.address
            )
          )
          .map(header =>
            BlockV2(
              header,
              BlockBodyV2(header.id, unsignedBlock.transactions)
            )
          )
  }
}
