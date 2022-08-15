package co.topl.genus

import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import org.mongodb.scala.Document
import org.scalacheck.Gen

object Generators extends ArbitraryInstances {

  val confirmedTransactionDataModelDocumentGen: Gen[Document] =
    confirmedTransactionDataModelArbitrary.arbitrary.map(_.asDocument)

  val blockDataModelDocumentGen: Gen[Document] = blockDataModelArbitrary.arbitrary.map(_.asDocument)
}
