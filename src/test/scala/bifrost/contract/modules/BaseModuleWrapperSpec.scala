package bifrost.contract.modules


import java.net.URLDecoder
import java.nio.file.{Path, Paths}

import bifrost.contract.BaseModuleWrapper
import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}


class BaseModuleWrapperSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val args: JsonObject = Map(
    "principal" -> "0".asJson,
    "accruedInterest" -> "0".asJson,
    "paymentInterval" -> "0".asJson,
    "interestRate" -> "0".asJson,
    "interestInterval" -> "0".asJson,
    "startingDate" -> "0".asJson,
    "totalPaid" -> "0".asJson,
    "lastPayment" -> Array("0", "0").map(_.asJson).asJson,
    "nextPayment" -> Array("0", "0").map(_.asJson).asJson
  ).asJsonObject

  val filePath = URLDecoder.decode(this.getClass.getResource("/contract-modules-fastopt.json").getPath, "UTF-8")
  val osAppropriatePath: Path = Paths.get(
    if (System.getProperty("os.name").contains("indow")) filePath.substring(1) else filePath
  )

  property("Json encoding and decoding should work") {
    val wrapper = BaseModuleWrapper(osAppropriatePath)(args)
    wrapper.json.as[BaseModuleWrapper].right.get shouldEqual wrapper
  }

}