package co.topl.utils

import co.topl.utils.NetworkType.{NetworkPrefix, PrivateTestnet}

trait NetworkPrefixTestHelper {

  implicit val networkPrefix: NetworkPrefix = PrivateTestnet.netPrefix
}
