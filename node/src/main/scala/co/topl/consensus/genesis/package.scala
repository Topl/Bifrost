package co.topl.consensus

package object genesis {
  trait Implicits extends GenesisProvider.ToGenesisProviderOps

  object implicits extends Implicits
}
