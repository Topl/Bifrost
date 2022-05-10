package co.topl.modifier

package object ops {

  trait Implicits extends TetraTransactionOps.Implicits with SimpleValueOps.Implicits with AssetValueOps.Implicits

  object implicits extends Implicits
}
