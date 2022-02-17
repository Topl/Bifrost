package co.topl.network

package object catsinstances {
  trait Implicits extends eqinstances.Implicits with showinstances.Implicits

  object implicits extends Implicits
}
