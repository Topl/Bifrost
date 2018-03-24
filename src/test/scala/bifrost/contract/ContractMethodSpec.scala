package bifrost.contract

import java.time.Instant

import bifrost.contract.Contract.Status
import bifrost.contract.Contract.Status.Status
import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

class ContractMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

}