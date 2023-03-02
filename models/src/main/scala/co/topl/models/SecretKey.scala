package co.topl.models

import co.topl.consensus.models.SignatureKesSum
import co.topl.models.utility.KesBinaryTree
import co.topl.models.utility.Lengths
import co.topl.models.utility.Sized

sealed trait SecretKey

object SecretKeys {
  case class VrfEd25519(bytes: Sized.Strict[Bytes, VrfEd25519.Length]) extends SecretKey

  object VrfEd25519 {
    type Length = Lengths.`32`.type // todo: check this value
  }

  case class KesSum(tree: KesBinaryTree, offset: Long) extends SecretKey

  case class KesProduct(
    superTree:    KesBinaryTree, // Hour hand
    subTree:      KesBinaryTree, // Minute hand
    nextSubSeed:  Array[Byte],
    subSignature: SignatureKesSum,
    offset:       Long
  ) extends SecretKey

}
