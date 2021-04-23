package co.topl.program

import io.circe.JsonObject
import org.scalacheck.Gen

import scala.util.Random

trait ProgramGenerators {

  val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val validExecutionBuilderTermsGen: Gen[ExecutionBuilderTerms] = for {
    size <- Gen.choose(1, 1024 - 1)
  } yield ExecutionBuilderTerms(Random.alphanumeric.take(size).mkString)

  def validInitJsGen(): Gen[String] = for {
    _ <- stringGen
  } yield s"""
             |var a = 0
             |
             |add = function() {
             |  a += 1
             |}
     """.stripMargin

  // TODO: This results in an empty generator far too often. Fix needed
  def validExecutionBuilderGen(): Gen[ExecutionBuilder] = for {
    assetCode <- stringGen
    terms     <- validExecutionBuilderTermsGen
    name      <- stringGen.suchThat(str => !Character.isDigit(str.charAt(0)))
    initjs    <- validInitJsGen()
  } yield ExecutionBuilder(terms, assetCode, ProgramPreprocessor(name, initjs)(JsonObject.empty))

  def sampleUntilNonEmpty[T](generator: Gen[T]): T = {
    var sampled = generator.sample

    while (sampled.isEmpty)
      sampled = generator.sample

    sampled.get
  }
}
