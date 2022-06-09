package co.topl.grpc

import akka.NotUsed
import akka.actor.ClassicActorSystemProvider
import akka.grpc.GrpcClientSettings
import akka.http.scaladsl.Http
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.{Chain, OptionT, ValidatedNec}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength, latin1DataLength}
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.language.implicitConversions

object ToplGrpc {

  object Client {

    /**
     * Creates a Topl RPC Client for interacting with a Bifrost node
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean)(implicit
      systemProvider:           ClassicActorSystemProvider
    ): F[ToplRpc[F, Source[*, NotUsed]]] =
      Async[F].delay {
        val client = services.ToplGrpcClient(GrpcClientSettings.connectToServiceAt(host, port).withTls(tls))
        new ToplRpc[F, Source[*, NotUsed]] {
          def broadcastTransaction(transaction: Transaction): F[Unit] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.broadcastTransaction(
                    services.BroadcastTransactionReq(transaction.immutableBytes)
                  )
                )
              )
              .void

          def blockAdoptions(): F[Source[TypedIdentifier, NotUsed]] =
            Async[F].delay(
              client
                .blockAdoptions(services.BlockAdoptionsReq())
                .map(_.blockId.asReadOnlyByteBuffer())
                .map(ByteVector(_))
                .map(TypedBytes(_))
            )

          def fetchHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.fetchBlockHeader(
                    services.FetchBlockHeaderReq(id.allBytes)
                  )
                )
              )
              .map(res =>
                res.header.map(h =>
                  BlockHeaderV2(
                    TypedBytes(h.parentHeaderId),
                    h.parentSlot,
                    Sized.strictUnsafe(h.txRoot: Bytes),
                    Sized.strictUnsafe(h.bloomFilter: Bytes),
                    h.timestamp,
                    h.height,
                    h.slot,
                    (h.eligibilityCertificate: Bytes).decodeImmutable[EligibilityCertificate].getOrElse(???),
                    (h.operationalCertificate: Bytes).decodeImmutable[OperationalCertificate].getOrElse(???),
                    h.metadata.some
                      .filter(_.nonEmpty)
                      .map(b => Latin1Data.fromData(b.toByteArray))
                      .map(Sized.maxUnsafe(_)),
                    (h.address: Bytes).decodeImmutable[StakingAddresses.Operator].getOrElse(???)
                  )
                )
              )

          def fetchBody(id: TypedIdentifier): F[Option[BlockBodyV2]] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.fetchBlockBody(
                    services.FetchBlockBodyReq(id.allBytes)
                  )
                )
              )
              .map(res => res.body.map(body => body.transactionIds.map(t => TypedBytes(t)).toList))

          def fetchTransaction(id: TypedIdentifier): F[Option[Transaction]] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.fetchTransaction(
                    services.FetchTransactionReq(id.allBytes)
                  )
                )
              )
              .map(_.transaction.map(decodeTransaction))

          private def decodeTransaction(transaction: services.Transaction): Transaction =
            Transaction(
              inputs = Chain.fromSeq(
                transaction.inputs.map(i =>
                  Transaction.Input(
                    i.boxId
                      .map(id => Box.Id(TypedBytes(id.transactionId), id.transactionOutputIndex.toShort))
                      .getOrElse(???),
                    (i.proposition: Bytes).decodeTransmitted[Proposition].getOrElse(???),
                    (i.proof: Bytes).decodeTransmitted[Proof].getOrElse(???),
                    decodeBoxValue(i.value)
                  )
                )
              ),
              outputs = Chain.fromSeq(
                transaction.outputs.map(o =>
                  Transaction.Output(
                    (o.address: Bytes).decodeTransmitted[FullAddress].getOrElse(???),
                    decodeBoxValue(o.value),
                    o.minting
                  )
                )
              ),
              chronology =
                transaction.chronology.map(c => Transaction.Chronology(c.creation, c.minimumSlot, c.maximumSlot)).get,
              transaction.metadataBytes.some
                .filter(_.nonEmpty)
                .map(b => Latin1Data.fromData(b.toByteArray))
                .map(Sized.maxUnsafe(_))
            )

          private def decodeBoxValue(value: services.BoxValue): Box.Value =
            value match {
              case services.BoxValue.Empty   => Box.Values.Empty
              case _: services.EmptyBoxValue => Box.Values.Empty
              case v: services.PolyBoxValue =>
                Box.Values.Poly(Sized.maxUnsafe(BigInt(v.quantity.toByteArray)))
              case v: services.ArbitBoxValue =>
                Box.Values.Arbit(Sized.maxUnsafe(BigInt(v.quantity.toByteArray)))
              case v: services.AssetBoxValue =>
                Box.Values.Asset(
                  Sized.maxUnsafe(BigInt(v.quantity.toByteArray)),
                  Box.Values.Asset.Code(
                    v.assetCode.get.version.toByte,
                    (v.assetCode.get.issuerAddress: Bytes)
                      .decodeTransmitted[SpendingAddress]
                      .getOrElse(???),
                    Sized.maxUnsafe[Latin1Data, Lengths.`8`.type](
                      Latin1Data.fromData(v.assetCode.get.shortName.toByteArray)
                    )
                  ),
                  Sized.strictUnsafe(v.securityRoot),
                  v.metadata.some
                    .filter(_.nonEmpty)
                    .map(b => Latin1Data.fromData(b.toByteArray))
                    .map(Sized.maxUnsafe(_))
                )
              case v: services.OperatorRegistrationBoxValue =>
                Box.Values.Registrations.Operator(
                  (v.vrfCommitment: Bytes).decodeTransmitted[Proofs.Knowledge.KesProduct].getOrElse(???)
                )
            }

          def currentMempool(): F[Set[TypedIdentifier]] =
            Async[F]
              .fromFuture(
                Async[F].delay(client.currentMempool(services.CurrentMempoolReq()))
              )
              .map(
                _.transactionIds.toList
                  .traverse[ValidatedNec[String, *], TypedIdentifier](data =>
                    (data: Bytes).decodeTransmitted[TypedIdentifier].toValidatedNec
                  )
                  .map(_.toSet)
                  .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
                  .toEither
              )
              .rethrow
        }
      }
  }

  object Server {

    /**
     * Serves the given ToplRpc interpreter over gRPC
     * @param host The host to bind
     * @param port The port to bind
     * @param interpreter The interpreter which fulfills the data requests
     */
    def serve[F[_]: Async: FToFuture](host: String, port: Int, interpreter: ToplRpc[F, Source[*, NotUsed]])(implicit
      systemProvider:                       ClassicActorSystemProvider
    ): Resource[F, Http.ServerBinding] =
      Resource.make(
        Async[F].fromFuture(
          Async[F].delay(
            Http()
              .newServerAt(host, port)
              .bind(services.ToplGrpcHandler(new GrpcServerImpl[F](interpreter)))
          )
        )
      )(binding => Async[F].fromFuture(Async[F].delay(binding.unbind())).void)

    private[grpc] class GrpcServerImpl[F[_]: MonadThrow: FToFuture](interpreter: ToplRpc[F, Source[*, NotUsed]])
        extends services.ToplGrpc {

      def broadcastTransaction(in: services.BroadcastTransactionReq): Future[services.BroadcastTransactionRes] =
        implicitly[FToFuture[F]].apply(
          (in.transmittableBytes: Bytes)
            .decodeTransmitted[Transaction]
            .leftMap(err => new IllegalArgumentException(s"Invalid Transaction bytes. reason=$err"))
            .liftTo[F]
            .flatMap(interpreter.broadcastTransaction)
            .as(services.BroadcastTransactionRes())
        )

      def currentMempool(in: services.CurrentMempoolReq): Future[services.CurrentMempoolRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .currentMempool()
            .map(ids => services.CurrentMempoolRes(ids.toList.map(_.transmittableBytes: ByteString)))
        )

      def blockAdoptions(in: services.BlockAdoptionsReq): Source[services.BlockAdoptionsRes, NotUsed] =
        Source
          .futureSource(implicitly[FToFuture[F]].apply(interpreter.blockAdoptions()))
          .map(_.allBytes.toByteBuffer)
          .map(ByteString.copyFrom)
          .map(services.BlockAdoptionsRes(_))
          .mapMaterializedValue(_ => NotUsed)

      def fetchBlockHeader(in: services.FetchBlockHeaderReq): Future[services.FetchBlockHeaderRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .fetchHeader(TypedBytes(in.blockId))
            .map(opt =>
              services.FetchBlockHeaderRes(
                opt.map(h =>
                  services.BlockHeader(
                    h.parentHeaderId.allBytes,
                    h.parentSlot,
                    h.txRoot.data,
                    h.bloomFilter.data,
                    h.timestamp,
                    h.height,
                    h.slot,
                    h.eligibilityCertificate.immutableBytes,
                    h.operationalCertificate.immutableBytes,
                    h.metadata.fold(Bytes.empty)(v => Bytes(v.data.bytes)),
                    h.address.immutableBytes
                  )
                )
              )
            )
        )

      def fetchBlockBody(in: services.FetchBlockBodyReq): Future[services.FetchBlockBodyRes] =
        implicitly[FToFuture[F]].apply(
          OptionT(interpreter.fetchBody(TypedBytes(in.blockId)))
            .map(body => services.BlockBody(body.map(_.allBytes)))
            .value
            .map(services.FetchBlockBodyRes(_))
        )

      def fetchTransaction(in: services.FetchTransactionReq): Future[services.FetchTransactionRes] =
        implicitly[FToFuture[F]].apply(
          OptionT(interpreter.fetchTransaction(TypedBytes(in.transactionId)))
            .fold(services.FetchTransactionRes(None, 0))(transaction =>
              services.FetchTransactionRes(
                services
                  .Transaction(
                    inputs = transaction.inputs
                      .map(i =>
                        services.Transaction.Input(
                          services.Box.Id(i.boxId.transactionId.allBytes, i.boxId.transactionOutputIndex).some,
                          i.proposition.transmittableBytes,
                          i.proof.transmittableBytes,
                          encodeBoxValue(i.value)
                        )
                      )
                      .toList,
                    outputs = transaction.outputs
                      .map(o =>
                        services.Transaction.Output(
                          o.address.transmittableBytes,
                          encodeBoxValue(o.value),
                          o.minting
                        )
                      )
                      .toList,
                    chronology = services.Transaction
                      .Chronology(
                        transaction.chronology.creation,
                        transaction.chronology.minimumSlot,
                        transaction.chronology.maximumSlot
                      )
                      .some,
                    metadataBytes = transaction.data.fold(Bytes.empty)(t => Bytes(t.data.bytes))
                  )
                  .some,
                transaction.immutableBytes.length.toInt
              )
            )
        )

      private def encodeBoxValue(value: Box.Value): services.BoxValue =
        value match {
          case Box.Values.Empty    => services.EmptyBoxValue()
          case v: Box.Values.Poly  => services.PolyBoxValue(ByteString.copyFrom(v.quantity.data.toByteArray))
          case v: Box.Values.Arbit => services.ArbitBoxValue(ByteString.copyFrom(v.quantity.data.toByteArray))
          case v: Box.Values.Asset =>
            services.AssetBoxValue(
              ByteString.copyFrom(v.quantity.data.toByteArray),
              services.AssetBoxValue
                .Code(
                  v.assetCode.version,
                  v.assetCode.issuer.transmittableBytes,
                  Bytes(v.assetCode.shortName.data.bytes)
                )
                .some,
              v.securityRoot.data,
              v.metadata.fold(Bytes.empty)(t => Bytes(t.data.bytes))
            )
          case v: Box.Values.Registrations.Operator =>
            services.OperatorRegistrationBoxValue(
              v.vrfCommitment.transmittableBytes
            )
        }
    }
  }

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def byteStringToByteVector(byteString: ByteString): Bytes =
    ByteVector(byteString.asReadOnlyByteBuffer())
}
