package co.topl.utils

import org.mongodb.scala.bson.Document
import simulacrum.typeclass

/**
 * Package containing codecs, typeclasses, and models for reading and writing data to Mongo DB.
 *
 * @example {{{
 * import co.topl.utils.mongodb.codecs._
 * import co.topl.utils.mongodb.implicits._
 *
 * // converting to document
 * val assetCode: AssetCode = ???
 * val assetCodeDataModel = AssetCodeDataModel(assetCode)
 * val assetCodeDocument = assetCodeDataModel.asDocument
 *
 * // converting from a document
 * val document = Document('{ "assetCodeVersion": 1, "issuer": "some address", "shortName": "my asset" }')
 * val model =
 *   DocumentDecoder[AssetCodeDataModel].fromDocument(document) // Either[String, AssetCodeDataModel]
 *
 * println(model)
 * // AssetCodeDataModel(assetCodeVersion: 1, issuer: "some address", shortName: "my asset")
 * }}}
 */
package object mongodb {

  @typeclass
  trait DocumentEncoder[T] {
    def asDocument(value: T): Document
  }

  @typeclass
  trait DocumentDecoder[T] {
    def fromDocument(document: Document): Either[String, T]
  }

  trait Implicits extends DocumentDecoder.ToDocumentDecoderOps with DocumentEncoder.ToDocumentEncoderOps

  object codecs extends Codecs

  object implicits extends Implicits
}
