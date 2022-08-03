package co.topl.modifier

package object ops {

  trait Implicits
      extends TetraTransactionOps.Implicits
      with DionTransactionOps.Implicits
      with SimpleValueOps.Implicits
      with AssetValueOps.Implicits
      with AssetOps.Implicits
      with AssetCodeOps.Implicits

  object implicits extends Implicits
}
