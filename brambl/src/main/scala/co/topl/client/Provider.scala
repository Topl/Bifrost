package co.topl.client

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import co.topl.akkahttprpc.RequestModifier
import co.topl.utils.NetworkType.{Mainnet, NetworkPrefix, PrivateTestnet, ValhallaTestnet}

sealed abstract class Provider {

  val uri: Uri
  val apiKey: String

  implicit val networkPrefix: NetworkPrefix

  implicit val requestModifier: RequestModifier =
    RequestModifier(
      _.withMethod(HttpMethods.POST)
        .withUri(uri)
        .withHeaders(RawHeader("x-api-key", apiKey))
    )
}

object Provider {

  class ToplMainNet(
    val uri:    Uri,
    val apiKey: String
  ) extends Provider {
    implicit val networkPrefix: NetworkPrefix = Mainnet.netPrefix
  }

  class ValhallaTestNet(
    val uri:    Uri,
    val apiKey: String
  ) extends Provider {
    implicit val networkPrefix: NetworkPrefix = ValhallaTestnet.netPrefix
  }

  class PrivateTestNet(
    val uri:    Uri = "http://localhost:9085",
    val apiKey: String
  ) extends Provider {
    implicit val networkPrefix: NetworkPrefix = PrivateTestnet.netPrefix
  }

  class Custom(val uri: Uri, val apiKey: String, val networkPrefix: NetworkPrefix) extends Provider
}
