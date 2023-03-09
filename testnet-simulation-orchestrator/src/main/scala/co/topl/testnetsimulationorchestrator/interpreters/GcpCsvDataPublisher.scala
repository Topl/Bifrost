package co.topl.testnetsimulationorchestrator.interpreters

import cats.effect._
import cats.implicits._
import co.topl.brambl.models.box.Value
import co.topl.codecs.bytes.tetra.instances._
import co.topl.numerics.implicits._
import co.topl.testnetsimulationorchestrator.algebras.DataPublisher
import co.topl.testnetsimulationorchestrator.models.AdoptionDatum
import co.topl.testnetsimulationorchestrator.models.BlockDatum
import co.topl.testnetsimulationorchestrator.models.TransactionDatum
import co.topl.typeclasses.implicits._
import com.google.cloud.storage._
import com.google.protobuf.ByteString
import fs2._

import java.nio.charset.StandardCharsets

object GcpCsvDataPublisher {

  def make[F[_]: Async](bucket: String, filePrefix: String): Resource[F, DataPublisher[F, Stream[F, *]]] =
    Resource
      .eval(
        Async[F].delay(
          StorageOptions.getUnauthenticatedInstance.getService
        )
      )
      .map(storage =>
        new DataPublisher[F, Stream[F, *]] {

          def publishAdoptions(results: Stream[F, AdoptionDatum], node: String): F[Unit] =
            publish[AdoptionDatum](node, AdoptionsCsvColumns)(adoptionDatumToRow)(results)

          def publishBlocks(results: Stream[F, BlockDatum]): F[Unit] =
            publish[BlockDatum]("blocks", BlocksCsvColumns)(blockDatumToRow)(results)

          def publishTransactions(results: Stream[F, TransactionDatum]): F[Unit] =
            publish[TransactionDatum]("transactions", TransactionsCsvColumns)(transactionDatumToRow)(results)

          // TODO: The Orchestrator may generate Transactions that never find their way into blocks (under poor network
          //  conditions, for example).  Information about Transactions that haven't been included in blocks
          //  could be useful for analysis.

          private def publish[Datum](fileName: String, csvHeaders: Seq[String])(
            datumToRow: Datum => Seq[String]
          )(results: Stream[F, Datum]) =
            upload(s"$filePrefix$fileName.csv")(
              Stream(csvHeaders.mkString("", ",", "\n"))
                .flatMap(header => Stream.chunk(Chunk.array(header.getBytes(StandardCharsets.UTF_8)))) ++
              results
                .map(datumToRow)
                .map(_.mkString("", ",", "\n"))
                .map(_.getBytes(StandardCharsets.UTF_8))
                .flatMap(bytes => Stream.chunk(Chunk.array(bytes)))
            )

          private def upload(fileName: String)(stream: Stream[F, Byte]): F[Unit] =
            stream.chunkAll.compile.toList
              .map(_.head.toByteBuffer.array())
              .flatMap(bytes =>
                Async[F].blocking(
                  // TODO: Stream this data to GCP.  Not currently streamed because of error when streaming to
                  //  GCP (potentially because of unauthenticated service).  If results are lost, look here!
                  storage.create(BlobInfo.newBuilder(bucket, fileName).setContentType("text/csv").build(), bytes)
                )
              )
        }
      )

  private val AdoptionsCsvColumns = List("id", "timestamp")

  private val BlocksCsvColumns = List(
    "id",
    "parentId",
    "parentSlot",
    "timestamp",
    "height",
    "slot",
    "address",
    "txRoot",
    "bloomFilter",
    "eligibilityCertificate",
    "operationalCertificate",
    "metadata",
    "transactions"
  )

  private val TransactionsCsvColumns = List(
    "id",
    "inputs",
    "outputs",
    "schedule",
    "dataLength"
  )

  private def adoptionDatumToRow(datum: AdoptionDatum) =
    List(datum.blockId.show, datum.timestamp.show)

  /**
   * TODO Implement immutableBytes in TetraIdentifiableInstances, see  TODO Qeustion about implementation
   */

  private def blockDatumToRow(datum: BlockDatum): List[String] =
    List(
      datum.header.id.show,
      datum.header.parentHeaderId.show,
      datum.header.parentSlot.show,
      datum.header.timestamp.show,
      datum.header.height.show,
      datum.header.slot.show,
      datum.header.address.show,
      datum.header.txRoot.show,
      datum.header.bloomFilter.show,
      datum.header.eligibilityCertificate.toByteString.show,
      datum.header.operationalCertificate.toByteString.show,
      datum.header.metadata.toString,
      datum.body.transactionIds.map(_.show).mkString(";")
    )

  private def transactionDatumToRow(datum: TransactionDatum) =
    List(
      datum.transaction.id.show,
      datum.transaction.inputs
        .map(input =>
          List(
            s"${input.address.getIoTransaction32.show}[${input.address.index}]",
            ByteString.EMPTY,
            ByteString.EMPTY,
            input.value.value match {
              case Value.Value.Empty    => "E"
              case v: Value.Value.Lvl   => s"L(${v.value.quantity: BigInt})"
              case v: Value.Value.Topl  => s"T(${v.value.quantity: BigInt})"
              case v: Value.Value.Asset => s"A(${v.value.quantity: BigInt})"
              case _                    => s"O"
            }
          ).mkString(":")
        )
        .mkString_(";"),
      datum.transaction.outputs
        .map(output =>
          List(
            output.address.getLock32.evidence.digest.value.show,
            output.value.value match {
              case Value.Value.Empty    => "E"
              case v: Value.Value.Lvl   => s"L(${v.value.quantity: BigInt})"
              case v: Value.Value.Topl  => s"T(${v.value.quantity: BigInt})"
              case v: Value.Value.Asset => s"A(${v.value.quantity: BigInt})"
              case _                    => s"O"
            }
          ).mkString(":")
        )
        .mkString_(";"),
      List(
        datum.transaction.datum.event.schedule.timestamp,
        datum.transaction.datum.event.schedule.min,
        datum.transaction.datum.event.schedule.max
      ).mkString(":"),
      datum.transaction.datum.event.metadata.value.show
    )
}
