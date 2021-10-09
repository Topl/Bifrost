package co.topl.crypto.signing

import co.topl.crypto.signing.kes.HdKesSymProdComp

class HdKesSymProd extends HdKesSymProdComp {}

object HdKesSymProd {
  val instance: HdKesSymProd = new HdKesSymProd
}