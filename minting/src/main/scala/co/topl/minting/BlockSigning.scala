package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.{BlockSigningAlgebra, KeyEvolverAlgebra}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, OperationalCertificate, Proofs, VerificationKeys}
import co.topl.typeclasses.BlockGenesis.zeroBytes
import co.topl.typeclasses.implicits._

object BlockSigning {

  object Eval {

    def make[F[_]: Monad](evolver: KeyEvolverAlgebra[F]): BlockSigningAlgebra[F] =
      unsignedBlock =>
        evolver
          .evolvedKey(unsignedBlock.unsignedHeader.slot)
          .map(evolvedKey => temporaryOpCert)
          .map(operationalCertificate =>
            BlockHeaderV2(
              unsignedBlock.unsignedHeader.parentHeaderId,
              unsignedBlock.unsignedHeader.parentSlot,
              unsignedBlock.unsignedHeader.txRoot,
              unsignedBlock.unsignedHeader.bloomFilter,
              unsignedBlock.unsignedHeader.timestamp,
              unsignedBlock.unsignedHeader.height,
              unsignedBlock.unsignedHeader.slot,
              unsignedBlock.unsignedHeader.eligibilityCertificate,
              operationalCertificate,
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

    // TODO: Generate a _real_ operational certificate
    private def temporaryOpCert =
      OperationalCertificate(
        opSig = Proofs.Signature.HdKes(
          i = 0,
          vkI = VerificationKeys.Ed25519(zeroBytes),
          ecSignature = Proofs.Signature.Ed25519(zeroBytes),
          sigSumJ = Proofs.Signature.SumProduct(
            ecSignature = Proofs.Signature.Ed25519(zeroBytes),
            vkK = VerificationKeys.Ed25519(zeroBytes),
            index = 0,
            witness = Nil
          ),
          sigSumK = Proofs.Signature.SumProduct(
            ecSignature = Proofs.Signature.Ed25519(zeroBytes),
            vkK = VerificationKeys.Ed25519(zeroBytes),
            index = 0,
            witness = Nil
          )
        ),
        xvkM = VerificationKeys.ExtendedEd25519(VerificationKeys.Ed25519(zeroBytes), zeroBytes),
        slotR = 0
      )
  }
}
