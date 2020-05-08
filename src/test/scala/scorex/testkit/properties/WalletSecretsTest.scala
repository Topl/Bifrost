package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.PersistentNodeViewModifier
import bifrost.transaction.box.proposition.Proposition
import bifrost.scorexMod.Wallet
import bifrost.transaction.bifrostTransaction.Transaction

trait WalletSecretsTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX]]
  extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {

  val wallet: Wallet[_, P, TX, PM, _]

  property("Wallet should contain secrets for all it's public propositions") {
    val publicImages = wallet.publicKeys
    assert(publicImages.nonEmpty, "please provide wallet with at least one secret")
    publicImages.foreach(pi => wallet.secretByPublicImage(pi).isDefined shouldBe true)
  }


}
