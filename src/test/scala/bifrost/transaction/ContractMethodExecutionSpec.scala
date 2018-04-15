package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import java.time.Instant

import bifrost.contract.Contract.Status
import bifrost.contract.{Agreement, Contract}
import bifrost.state.BifrostState
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.Longs
import io.circe.Json
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Random

class ContractMethodExecutionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

}
