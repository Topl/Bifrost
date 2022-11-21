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
import co.topl.models.Box

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

          private def publish[Datum](fileName: String, headers: Seq[String])(
            datumToRow:                        Datum => Seq[String]
          )(results:                           Stream[F, Datum]) =
            upload(s"$filePrefix$fileName.csv")(
              Stream(headers.mkString("", ",", "\n"))
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

  private def blockDatumToRow(datum: BlockDatum): List[String] =
    List(
      datum.headerV2.id.asTypedBytes.show,
      datum.headerV2.parentHeaderId.show,
      datum.headerV2.parentSlot.show,
      datum.headerV2.timestamp.show,
      datum.headerV2.height.show,
      datum.headerV2.slot.show,
      datum.headerV2.address.immutableBytes.toBase58,
      datum.headerV2.txRoot.data.toBase58,
      datum.headerV2.bloomFilter.data.toBase58,
      datum.headerV2.eligibilityCertificate.immutableBytes.toBase58,
      datum.headerV2.operationalCertificate.immutableBytes.toBase58,
      datum.headerV2.metadata.fold("")(_.data.value),
      datum.bodyV2.map(_.show).mkString(";")
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
          ).mkString(";")
        )
        .mkString_(":"),
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
          ).mkString(";")
        )
        .mkString_(":"),
      List(
        datum.transaction.schedule.creation,
        datum.transaction.schedule.minimumSlot,
        datum.transaction.schedule.maximumSlot
      ).mkString(":"),
      datum.transaction.data.fold(0L)(_.length).toString
    )
}
