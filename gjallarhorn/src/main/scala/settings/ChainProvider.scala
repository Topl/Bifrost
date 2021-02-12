package settings

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pureconfig._
import pureconfig.generic.semiauto._

sealed abstract class ChainProvider(val chainProvider: String,
                                    val name: String,
                                    val networkName: String)

object ChainProvider {
  implicit val chainProviderReader: ConfigReader[ChainProvider] = deriveReader[ChainProvider]

  implicit val jsonEncoder: Encoder[ChainProvider] = {
    case cp: AkkaChainProvider => AkkaChainProvider.jsonEncoder(cp)
    case cp: HttpChainProvider  => HttpChainProvider.jsonEncoder(cp)
    case _              => throw new Error(s"No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[ChainProvider] = { c: HCursor =>
    c.downField("type").as[String].map {
      case AkkaChainProvider.mode => AkkaChainProvider.jsonDecoder(c)
      case HttpChainProvider.mode => HttpChainProvider.jsonDecoder(c)
    } match {
      case Right(cp) => cp
      case Left(ex) => throw ex
    }
  }
}

case class AkkaChainProvider(override val chainProvider: String,
                             override val name: String,
                             override val networkName: String
                            ) extends ChainProvider(chainProvider, name, networkName)

object AkkaChainProvider {
  val mode: String = "Akka"
  implicit val akkaChainProviderReader: ConfigReader[AkkaChainProvider] = deriveReader[AkkaChainProvider]

  implicit val jsonEncoder: Encoder[AkkaChainProvider] = { (cp: AkkaChainProvider) =>
    Map(
      "type"          -> mode.asJson,
      "chainProvider" -> cp.chainProvider.asJson,
      "name"          -> cp.name.asJson,
      "network"       -> cp.networkName.asJson,
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AkkaChainProvider] = (c: HCursor) =>
    for {
      chainProvider   <- c.downField("chainProvider").as[String]
      name            <- c.downField("name").as[String]
      networkName     <- c.downField("network").as[String]
    } yield {
      NetworkType.fromString(networkName) match {
        case Some(_) => AkkaChainProvider(chainProvider, name, networkName)
        case None => throw new Exception (s"Invalid network name: $networkName")
      }
    }
}

case class HttpChainProvider(override val chainProvider: String,
                             override val name: String,
                             override val networkName: String,
                             var apiKey: String
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
      chainProvider     <- c.downField("chainProvider").as[String]
      name    <- c.downField("name").as[String]
      networkName <- c.downField("network").as[String]
      apiKey <- c.downField("apiKey").as[String]
    } yield {
      NetworkType.fromString(networkName) match {
        case Some(_) => HttpChainProvider(chainProvider, name, networkName, apiKey)
        case None => throw new Exception (s"Invalid network name: $networkName")
      }
    }
}