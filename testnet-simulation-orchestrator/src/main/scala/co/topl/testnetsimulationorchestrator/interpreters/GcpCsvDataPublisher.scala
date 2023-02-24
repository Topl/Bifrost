package co.topl.testnetsimulationorchestrator.interpreters

import fs2._
import cats.implicits._
import cats.effect._
import co.topl.testnetsimulationorchestrator.algebras.DataPublisher
import co.topl.testnetsimulationorchestrator.models.{AdoptionDatum, BlockDatum, TransactionDatum}
import com.google.cloud.storage._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models.{Box, TypedIdentifier}
import co.topl.models.utility._

import java.nio.charset.StandardCharsets
import scodec.bits.ByteVector

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
      datum.header.id.asTypedBytes.show,
      (datum.header.parentHeaderId: TypedIdentifier).show,
      datum.header.parentSlot.show,
      datum.header.timestamp.show,
      datum.header.height.show,
      datum.header.slot.show,
      (datum.header.address: ByteVector).toBase58,
      (datum.header.txRoot: ByteVector).toBase58,
      (datum.header.bloomFilter: ByteVector).toBase58,
      datum.header.eligibilityCertificate.immutableBytes.toBase58,
      datum.header.operationalCertificate.immutableBytes.toBase58,
      datum.header.metadata.toString,
      datum.body.transactionIds.map(t => t: TypedIdentifier).map(_.show).mkString(";")
    )

  private def transactionDatumToRow(datum: TransactionDatum) =
    List(
      datum.transaction.id.asTypedBytes.show,
      datum.transaction.inputs
        .map(input =>
          List(
            s"${input.boxId.transactionId.show}[${input.boxId.transactionOutputIndex}]",
            input.proposition.immutableBytes.toBase58,
            input.proof.immutableBytes.toBase58,
            input.value match {
              case Box.Values.Empty      => "E"
              case v: Box.Values.Poly    => s"P(${v.quantity.data})"
              case v: Box.Values.Arbit   => s"Ar(${v.quantity.data})"
              case v: Box.Values.AssetV1 => s"As(${v.quantity.data})"
              case _                     => s"O"
            }
          ).mkString(":")
        )
        .mkString_(";"),
      datum.transaction.outputs
        .map(output =>
          List(
            output.address.immutableBytes.toBase58,
            output.value match {
              case Box.Values.Empty      => "E"
              case v: Box.Values.Poly    => s"P(${v.quantity.data})"
              case v: Box.Values.Arbit   => s"Ar(${v.quantity.data})"
              case v: Box.Values.AssetV1 => s"As(${v.quantity.data})"
              case _                     => s"O"
            },
            output.minting.show
          ).mkString(":")
        )
        .mkString_(";"),
      List(
        datum.transaction.schedule.creation,
        datum.transaction.schedule.minimumSlot,
        datum.transaction.schedule.maximumSlot
      ).mkString(":"),
      datum.transaction.data.fold(0L)(_.length).toString
    )
}
