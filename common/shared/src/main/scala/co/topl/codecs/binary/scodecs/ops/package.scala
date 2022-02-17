package co.topl.codecs.binary.scodecs

package object ops {
  trait Implicits extends ScodecOps.ToScodecOps
  object implicits extends Implicits
}
