package co.topl.grpc

import cats.Monad
import cats.data.ValidatedNec
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.{models => bifrostModels}
import co.topl.models.utility.HasLength.instances.{bytesLength, latin1DataLength}
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import com.google.protobuf.ByteString

import scala.collection.immutable.ListSet

/**
 * Represents a fallible isomorphism between two types, A and B.
 */
trait MessageIsomorphism[F[_], A, B] {
  def aToB(a: F[A]): F[Either[String, B]]
  def bToA(b: F[B]): F[Either[String, A]]
}

object MessageIsomorphism {

  def create[F[_], A, B](
    _aToB: F[A] => F[Either[String, B]],
    _bToA: F[B] => F[Either[String, A]]
  ): MessageIsomorphism[F, A, B] =
    new MessageIsomorphism[F, A, B] {
      def aToB(a: F[A]): F[Either[String, B]] = _aToB(a)

      def bToA(b: F[B]): F[Either[String, A]] = _bToA(b)
    }

  trait Instances {

    implicit def headerIsomorphism[F[_]: Monad]
      : MessageIsomorphism[F, bifrostModels.BlockHeaderV2, services.BlockHeader] =
      MessageIsomorphism.create(
        _.map(header =>
          services
            .BlockHeader(
              header.parentHeaderId.transmittableBytes,
              header.parentSlot,
              header.txRoot.data,
              header.bloomFilter.data,
              header.timestamp,
              header.height,
              header.slot,
              header.eligibilityCertificate.transmittableBytes,
              header.operationalCertificate.transmittableBytes,
              header.metadata.map(v => services.BlockHeader.Metadata(bifrostModels.Bytes(v.data.bytes))),
              header.address.transmittableBytes
            )
            .asRight
        ),
        _.map(protoHeader =>
          for {
            txRoot <- Sized
              .strict[bifrostModels.Bytes, Lengths.`32`.type](protoHeader.txRoot: bifrostModels.Bytes)
              .leftMap(_ => "Invalid txRoot")
            bloomFilter <- Sized
              .strict[bifrostModels.Bytes, Lengths.`256`.type](protoHeader.bloomFilter: bifrostModels.Bytes)
              .leftMap(_ => "Invalid bloomFilter")
            eligibilityCertificate <- (protoHeader.eligibilityCertificate: bifrostModels.Bytes)
              .decodeTransmitted[bifrostModels.EligibilityCertificate]
              .leftMap(_ => "Invalid eligibilityCertificate")
            operationalCertificate <- (protoHeader.operationalCertificate: bifrostModels.Bytes)
              .decodeTransmitted[bifrostModels.OperationalCertificate]
              .leftMap(_ => "Invalid operationalCertificate")
            metadata <- protoHeader.metadata.traverse(metadata =>
              Sized
                .max[Latin1Data, Lengths.`32`.type](Latin1Data.fromData(metadata.toByteArray))
                .leftMap(_ => "Invalid metadata")
            )
            address <- (protoHeader.address: bifrostModels.Bytes)
              .decodeTransmitted[bifrostModels.StakingAddresses.Operator]
              .leftMap(_ => "Invalid address")
          } yield bifrostModels.BlockHeaderV2(
            bifrostModels.TypedBytes(protoHeader.parentHeaderId),
            protoHeader.parentSlot,
            txRoot,
            bloomFilter,
            protoHeader.timestamp,
            protoHeader.height,
            protoHeader.slot,
            eligibilityCertificate,
            operationalCertificate,
            metadata,
            address
          )
        )
      )

    implicit def bodyIsomorphism[F[_]: Monad]: MessageIsomorphism[F, bifrostModels.BlockBodyV2, services.BlockBody] =
      MessageIsomorphism.create(
        _.map(body => services.BlockBody(body.toList.map(_.transmittableBytes: ByteString)).asRight),
        _.map(protoBody =>
          protoBody.transactionIds.toList
            .traverse[ValidatedNec[String, *], bifrostModels.TypedIdentifier](data =>
              (data: bifrostModels.Bytes).decodeTransmitted[bifrostModels.TypedIdentifier].toValidatedNec
            )
            .map(ListSet.empty[bifrostModels.TypedIdentifier] ++ _)
            .leftMap(errors => show"Invalid block body. reason=$errors")
            .toEither
        )
      )
  }

  trait Ops {

    implicit class IsomorphicValueOps[F[_], X](x: F[X]) {

      def toB[B](implicit isomorphim: MessageIsomorphism[F, X, B]): F[Either[String, B]] =
        isomorphim.aToB(x)

      def toA[A](implicit isomorphim: MessageIsomorphism[F, A, X]): F[Either[String, A]] =
        isomorphim.bToA(x)
    }
  }

  object instances extends Instances with Ops
}
