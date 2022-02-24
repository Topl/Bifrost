package co.topl.modifier

package object ops {
  trait Implicits extends DionTransactionOps.Implicits with TetraTransactionOps.Implicits

  object implicits extends Implicits
}
