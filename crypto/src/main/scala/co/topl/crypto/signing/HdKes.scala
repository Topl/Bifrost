package co.topl.crypto.signing

import co.topl.crypto.signing.kes.HdKesComp

class HdKes extends HdKesComp {}

object HdKes {
  val instance: HdKes = new HdKes
}