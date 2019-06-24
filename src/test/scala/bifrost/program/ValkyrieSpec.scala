package bifrost.program

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}

class ValkyrieSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val testValkyrie =
    s"""
       |this.assetCreated = {};
       |this.assetTransferred = {};
       |this.polyTransferred = {};
       |
       |this.createAssets = function(issuer, to, amount, assetCode, fee, data) {
       |  this.issuer = issuer;
       |  this.to = to;
       |  this.amount = amount;
       |  this.assetCode = assetCode;
       |  this.fee = fee;
       |  this.data = data;
       |
       |  return assetCreated;
       |}
       |
       |function create() {
       |  a = 2 + 2;
       |  var res = createAssets("a", "b", 10, "testAssets", 0, "");
       |  }
     """.stripMargin




}
