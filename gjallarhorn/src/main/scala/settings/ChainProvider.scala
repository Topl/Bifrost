package settings

/**
 * Abstract class for [[HttpChainProvider]] and [[AkkaChainProvider]].
 * Represents information for a chain provider, which is the information needed to connect/communicate with Bifrost.
 * @param chainProvider - the address for Bifrost
 * @param name - the name of the chain provider
 * @param networkName - the network name for the connection.
 */
sealed abstract class ChainProvider(val chainProvider: String, val name: String, val networkName: String)

object ChainProvider {
  implicit val chainProviderReader: ConfigReader[ChainProvider] = deriveReader[ChainProvider]

  implicit val jsonEncoder: Encoder[ChainProvider] = {
    case cp: AkkaChainProvider => AkkaChainProvider.jsonEncoder(cp)
    case cp: HttpChainProvider => HttpChainProvider.jsonEncoder(cp)
    case _                     => throw new Error(s"No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[ChainProvider] = { c: HCursor =>
    c.downField("type").as[String].map {
      case AkkaChainProvider.mode => AkkaChainProvider.jsonDecoder(c)
      case HttpChainProvider.mode => HttpChainProvider.jsonDecoder(c)
    } match {
      case Right(cp) => cp
      case Left(ex)  => throw ex
    }
  }
}

/**
 * A chain provider used to connect to Bifrost through the akka actor system.
 * @param chainProvider - Bifrost's akka remote address
 * @param name - the name of the chain provider
 * @param networkName - the network name for the connection.
 */
case class AkkaChainProvider(
  override val chainProvider: String,
  override val name:          String,
  override val networkName:   String
) extends ChainProvider(chainProvider, name, networkName)

object AkkaChainProvider {
  val mode: String = "Akka"
  implicit val akkaChainProviderReader: ConfigReader[AkkaChainProvider] = deriveReader[AkkaChainProvider]

  implicit val jsonEncoder: Encoder[AkkaChainProvider] = { (cp: AkkaChainProvider) =>
    Map(
      "type"          -> mode.asJson,
      "chainProvider" -> cp.chainProvider.asJson,
      "name"          -> cp.name.asJson,
      "network"       -> cp.networkName.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AkkaChainProvider] = (c: HCursor) =>
    for {
      chainProvider <- c.downField("chainProvider").as[String]
      name          <- c.downField("name").as[String]
      networkName   <- c.downField("network").as[String]
    } yield NetworkType.fromString(networkName) match {
      case Some(_) => AkkaChainProvider(chainProvider, name, networkName)
      case None    => throw new Exception(s"Invalid network name: $networkName")
    }
}

/**
 * Chain provider used to communicate with Bifrost through Http
 * @param chainProvider - the address for Bifrost's API
 * @param name - the name of the chain provider
 * @param networkName - the network name for the connection.
 * @param apiKey - Bifrost's api key
 */
case class HttpChainProvider(
  override val chainProvider: String,
  override val name:          String,
  override val networkName:   String,
  var apiKey:                 String
) extends ChainProvider(chainProvider, name, networkName)

object HttpChainProvider {
  val mode: String = "Http"
  implicit val httpChainProviderReader: ConfigReader[HttpChainProvider] = deriveReader[HttpChainProvider]

  implicit val jsonEncoder: Encoder[HttpChainProvider] = { (cp: HttpChainProvider) =>
    Map(
      "type"          -> mode.asJson,
      "chainProvider" -> cp.chainProvider.asJson,
      "name"          -> cp.name.asJson,
      "network"       -> cp.networkName.asJson,
      "apiKey"        -> cp.apiKey.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[HttpChainProvider] = (c: HCursor) =>
    for {
      chainProvider <- c.downField("chainProvider").as[String]
      name          <- c.downField("name").as[String]
      networkName   <- c.downField("network").as[String]
      apiKey        <- c.downField("apiKey").as[String]
    } yield NetworkType.fromString(networkName) match {
      case Some(_) => HttpChainProvider(chainProvider, name, networkName, apiKey)
      case None    => throw new Exception(s"Invalid network name: $networkName")
    }
}
