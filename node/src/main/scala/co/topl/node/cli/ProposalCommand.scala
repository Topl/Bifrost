package co.topl.node.cli

import cats.effect.Async
import cats.effect.std.Console
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event, LockAddress}
import co.topl.node.cli.ProposalCommand.Messages
import quivr.models.Ratio
import com.google.protobuf.duration.Duration
import fs2.io.file.Path

object ProposalCommand {

  def apply[F[_]: Async: Console]: StageResultT[F, Unit] = new ProposalCommandImpl[F].command

  object Messages {

    val intro =
      "This tool will guide you through the process of preparing a Proposal Update. Press <Enter> to skip."

    val lockAddress = "Using a wallet (i.e. Brambl CLI), create or provide a new LockAddress."

    val writeTransaction = "This process will save the Proposal update in the current /tmp path"

    val finalMsg =
      "Your Update proposal has been saved. The Transaction that was saved should be imported into Brambl-CLI for input selection and broadcast."
  }
}

/**
 * Proposal Update command used by node cli app
 */
private class ProposalCommandImpl[F[_]: Async](implicit c: Console[F]) {

  val command: StageResultT[F, Unit] =
    for {
      _ <- writeMessage[F](Messages.intro)

      label                      <- readParameter[F, String]("label <required>", List("Update Slot duration"))
      fEffective                 <- readOptionalParameter[F, Ratio]("f-effective", List("15/100"))
      vrfLddCutoff               <- readOptionalParameter[F, Int]("vrf-ldd-cutoff", List("50"))
      vrfPrecision               <- readOptionalParameter[F, Int]("vrf-precision", List("40"))
      vrfBaselineDifficulty      <- readOptionalParameter[F, Ratio]("vrf-baseline-difficulty", List("1/20"))
      vrfAmplitude               <- readOptionalParameter[F, Ratio]("vrf-amplitude", List("1/2"))
      chainSelectionKLookback    <- readOptionalParameter[F, Long]("chain-selection-k-lookback", List("50"))
      slotDuration               <- readOptionalParameter[F, Duration]("slot-duration", List("1000 milli"))
      forwardBiasedSlotWindow    <- readOptionalParameter[F, Long]("forward-biased-slot-window", List("50"))
      operationalPeriodsPerEpoch <- readOptionalParameter[F, Long]("operational-periods-per-epoch", List("2"))
      kesKeyHours                <- readOptionalParameter[F, Int]("kes-key-hours", List("2"))
      kesKeyMinutes              <- readOptionalParameter[F, Int]("kes-key-minutes", List("9"))

      proposal = UpdateProposal(
        label,
        fEffective,
        vrfLddCutoff,
        vrfPrecision,
        vrfBaselineDifficulty,
        vrfAmplitude,
        chainSelectionKLookback,
        slotDuration,
        forwardBiasedSlotWindow,
        operationalPeriodsPerEpoch,
        kesKeyHours,
        kesKeyMinutes
      )

      lockAddress <- readParameter[F, LockAddress](
        "Address",
        List("ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr")
      )

      _ <- writeMessage[F](Messages.lockAddress)
      //      _ <- requiresBramblCli // TODO refactor this method and print this message for BramblCLI

      unspentTransactionOutput =
        UnspentTransactionOutput(lockAddress, Value.defaultInstance.withUpdateProposal(proposal))

      transaction = IoTransaction(datum =
        Datum.IoTransaction(
          Event.IoTransaction.defaultInstance.withSchedule(
            Schedule(0L, Long.MaxValue, System.currentTimeMillis())
          )
        )
      ).withOutputs(Seq(unspentTransactionOutput))

      // TODO The name should be the id of the proposal Updated, path should be a user input
      _ <- writeFile[F](Path("/tmp"))(transaction.toByteArray)(
        "Proposal Update",
        "proposalUpdate.transaction.pbuf"
      )
      _ <- writeMessage[F](Messages.writeTransaction)
      _ <- writeMessage[F](Messages.finalMsg)
    } yield StageResult.Menu

}
