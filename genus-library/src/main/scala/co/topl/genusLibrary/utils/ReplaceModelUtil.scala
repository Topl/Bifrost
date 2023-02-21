package co.topl.genusLibrary.utils

import co.topl.brambl.models.{Datum, Evidence, Identifier, KnownIdentifier}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{Attestation, IoTransaction, SpentTransactionOutput}
import quivr.models.Digest.Digest32

/**
 * Do remove when Grpc fetch trasaction returns IoTransaction
 */
object ReplaceModelUtil {

  /**
   * This replaceLegacyModel only contains some replacements, do not expose it on ReplaceModelUtil
   */
  def replaceTransactionLegacyModel(legacyTransaction: co.topl.proto.models.Transaction): IoTransaction =
    IoTransaction.of(
      inputs = legacyTransaction.inputs.map(input =>
        SpentTransactionOutput.of(
          knownIdentifier = KnownIdentifier.of(value =
            KnownIdentifier.Value.TransactionOutput32(
              KnownIdentifier.TransactionOutput32.of(
                network = 0,
                ledger = 0,
                index = input.boxId.get.transactionOutputIndex,
                id = Identifier.IoTransaction32.of(
                  Evidence.Sized32.of(
                    Digest32.of(input.boxId.get.transactionId.get.value)
                  )
                )
              )
            )
          ),
          attestation = Attestation.defaultInstance,
          value = Value.defaultInstance,
          datum = Datum.SpentOutput.defaultInstance,
          opts = Seq.empty
        )
      ),
      outputs = Seq.empty,
      datum = Datum.IoTransaction.defaultInstance
    )

}
