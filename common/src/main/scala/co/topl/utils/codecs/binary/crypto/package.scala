package co.topl.utils.codecs.binary

package object crypto {
  trait Codecs extends PublicKeyCodec.Codecs with SignatureCodec.Codecs

  object codecs extends Codecs
}
