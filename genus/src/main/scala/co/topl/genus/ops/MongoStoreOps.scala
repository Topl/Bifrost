package co.topl.genus.ops

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.algebras.MongoStore
import co.topl.genus.services.services_types.Paging
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

class MongoStoreOps[F[_]](private val store: MongoStore[F]) extends AnyVal {

  def getDocumentsWithPaging(
    filter: Option[Bson],
    sort:   Option[Bson],
    paging: Option[Paging]
  ): F[Source[Document, NotUsed]] =
    store.getDocuments(filter, sort, paging.map(_.pageSize), paging.map(p => p.pageSize * p.pageNumber))
}

object MongoStoreOps {

  trait ToOps {
    implicit def mongoStoreOpsFromValue[F[_]](value: MongoStore[F]): MongoStoreOps[F] = new MongoStoreOps[F](value)
  }

  object ops extends ToOps
}
