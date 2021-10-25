package co.topl.utils

import org.mongodb.scala.bson.Document
import simulacrum.typeclass

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
