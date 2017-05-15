package bifrost.contract

import java.security.InvalidParameterException
import java.time.Instant

import com.google.common.primitives.Longs
import io.circe._
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import sun.plugin.dom.exception.InvalidStateException

import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}


class Contract(val Producer: PublicKey25519Proposition,
               val Hub: PublicKey25519Proposition,
               val Investor: PublicKey25519Proposition,
               val storage: JsonObject,
               val agreement: JsonObject,
               val id: Array[Byte]) {

  lazy val json: Json = Map(
    "producer" -> Base58.encode(Producer.pubKeyBytes).asJson,
    "hub" -> Base58.encode(Hub.pubKeyBytes).asJson,
    "investor" -> Base58.encode(Investor.pubKeyBytes).asJson,
    "agreement" -> agreement.asJson,
    "storage" -> storage.asJson
  ).asJson

  def complete(party: PublicKey25519Proposition)(): Try[Contract] = {
    if (Producer.pubKeyBytes sameElements party.pubKeyBytes) {
      Success(this)
    } else if (Hub.pubKeyBytes sameElements party.pubKeyBytes) {
      Success(this)
    } else if (Investor.pubKeyBytes sameElements party.pubKeyBytes) {
      Success(this)
    } else {
      Failure(new IllegalAccessException("Actor doesn't correspond to any"))
    }

  }

  def currentStatus(party: PublicKey25519Proposition)(): Try[Json] = Try {
    storage("status").get
  }

  def deliver(party: PublicKey25519Proposition)(quantity: Long): Try[Contract] = {

    require(party.pubKeyBytes sameElements Producer.pubKeyBytes, Failure(new IllegalAccessException("Wrong actor")))
    require(quantity > 0L, Failure(new IllegalArgumentException(s"Delivery quantity <$quantity> must be positive")))

    val status: String = storage("status").get.asString.get

    require(!status.equals("expired") && !status.equals("complete"), Failure(new IllegalStateException(s"Contract state <$status> is invalid")))

    val currentFulfillmentJsonObj: JsonObject = storage("currentFulfillment").getOrElse(Map("deliveredQuantity" -> 0L.asJson).asJson).asObject.get
    val newFulfillmentJsonObj: JsonObject = currentFulfillmentJsonObj.add(
      "deliveredQuantity",
      (currentFulfillmentJsonObj("deliveredQuantity").getOrElse(0L.asJson).asNumber.get.toLong.get + quantity).asJson
    )
    val newStorage = storage.add("currentFulfillment", newFulfillmentJsonObj.asJson)

    Success(new Contract(Producer, Hub, Investor, newStorage, agreement, id))
  }

  def confirmDelivery(party: PublicKey25519Proposition)(): Try[Contract] = {
    if (party.pubKeyBytes sameElements Hub.pubKeyBytes) {
      Success(this)
    } else {
      Failure(new IllegalAccessException("Wrong actor"))
    }

  }

  def checkExpiration(party: PublicKey25519Proposition)(): Try[Json] = Try {
    val expiration: Long = this.agreement("expirationTimestamp").get.as[Long].right.get
    (Instant.now().toEpochMilli > expiration).asJson
  }

}

object Contract {

  // get runtime mirror
  val rm = ru.runtimeMirror(getClass.getClassLoader)

  // TODO this currently also shows public accessors and the like. Want to restrict. May have to build registry after all
  val contractMethods: Map[String, ru.MethodSymbol] = ru.typeOf[Contract].decls collect {
    case m: ru.Symbol if m.isMethod => m.asMethod.name.toString -> m.asMethod
  } toMap

  def apply(cs: Json, id: Array[Byte]): Contract = {
    val jsonMap = cs.asObject.get.toMap

    new Contract(
      new PublicKey25519Proposition(Base58.decode(jsonMap("producer").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("hub").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("investor").asString.get).get),
      jsonMap("storage").asObject.get,
      jsonMap("agreement").asObject.get,
      id
    )
  }

  //noinspection ScalaStyle
  def execute(c: Contract, methodName: String)(party: PublicKey25519Proposition)(args: JsonObject): Try[Either[Contract, Json]] = Try {

    val methodAttempt: Option[ru.MethodSymbol] = contractMethods.get(methodName)

    methodAttempt match {
      case Some(m: ru.MethodSymbol) =>
        val params: List[Any] = m.paramLists.map(p => {
          val typename = p.head.typeSignature.typeSymbol.asClass.name.toString
          p.head.name.toString match {
            case "party" => party
            case _ => typename match {
              case "PublicKey25519Proposition" => PublicKey25519Proposition(Base58.decode(args(p.head.name.toString).get.asString.get).get)
              case "Long" => args(p.head.name.toString).get.asNumber.get.toLong.get
              case i => throw new NotImplementedError(s"Decoder for datatype $i not implemented")
            }
          }
        })

        rm.reflect(c).reflectMethod(m)(params:_*) match {
          case c: Success[Contract] => Left(c.value)
          case j: Success[Json] => Right(j.value)
          case f: Failure[Any] => throw f.exception
        }

      case _ => throw new MatchError(s"Could not find method <$methodName>")
    }
  }

}