package co.topl.utils

package object catsinstances {
  trait Implicits extends EqInstances with ShowInstances

  object implicits extends Implicits
}
