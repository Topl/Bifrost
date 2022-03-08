package co.topl.modifier.transaction.builder

package object ops {

  trait Implicits extends BoxSetOps.Implicits with CoinOutputsOps.Implicits

  object implicits extends Implicits
}
