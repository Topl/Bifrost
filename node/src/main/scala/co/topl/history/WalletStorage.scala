package co.topl.history

import co.topl.primitives.{ByteStream, Fch, LDBStore, SimpleTypes}
import io.iohk.iodb.ByteArrayWrapper
import co.topl.components
import co.topl.components.{Serializer, Wallet}

import scala.util.Try

/**
  * AMS 2020:
  * Wallet is saved to disk on reorg
  */

class WalletStorage(dir:String) extends SimpleTypes {
  import co.topl.components.Serializer._
  val fch = new Fch
  var walletStore:LDBStore = LDBStore(s"$dir/wallet")

  def refresh():Unit = {
    walletStore.refresh()
  }

  def restore(serializer: Serializer,pkw:ByteArrayWrapper):Wallet = {
    def newWallet:Wallet = {
      println("New wallet")
      val out = components.Wallet(pkw)
      store(out,serializer)
      out
    }
    walletStore.get(ByteArrayWrapper(fch.hash(pkw.data))) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream = new ByteStream(bytes.data,DeserializeWallet)
        Try{serializer.fromBytes(byteStream)}.toOption match {
          case Some(w:Wallet) if w.pkw == pkw =>
            println("Recovered wallet")
            w
          case _ => newWallet
        }
      }
      case _ => newWallet
    }
  }

  def store(wallet:Wallet,serializer: Serializer):Unit = {
    val wBytes = serializer.getBytes(wallet)
    val key = ByteArrayWrapper(fch.hash(wallet.pkw.data))
    walletStore.update(Seq(),Seq(key -> ByteArrayWrapper(wBytes)))
  }

}
