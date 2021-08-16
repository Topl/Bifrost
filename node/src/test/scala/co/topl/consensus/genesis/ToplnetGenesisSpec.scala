package co.topl.consensus.genesis

import co.topl.modifier.box.TokenBox
import co.topl.utils.Int128
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Success

class ToplnetGenesisSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("The Toplnet genesis block should contain genesis transactions creating the specified total of coins") {
    val totalCoins: Int128 = ToplnetGenesis.members.values.sum

    val (arbitTotal, polyTotal) = ToplnetGenesis.getGenesisBlock.map { case (block, _) =>
      block.transactions.map { tx =>
        tx.newBoxes.map { case box: TokenBox[_] =>
          box.value.quantity
        }.sum
      }
    } match {
      case Success(arbitTotal :: polyTotal) => (arbitTotal, polyTotal.head)
    }

    arbitTotal shouldBe totalCoins
    polyTotal shouldBe totalCoins
  }
}
