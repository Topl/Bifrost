package co.topl.stakeholder.components

import co.topl.stakeholder.primitives._

/**
  * AMS 2020:
  * Serializer class for actors
  */

class Serializer extends SimpleTypes with SerializationMethods

/**
  * Deserialize pattern matching, feel free to add distinct case objects for new classes to be parsed
  */

object Serializer {
  case object DeserializeBlock
  case object DeserializeGenesisBlock
  case object DeserializeBlockHeader
  case object DeserializeTransaction
  case object DeserializeMac
  case object DeserializeGenesisSet
  case object DeserializeTransactionSet
  case object DeserializeIdList
  case object DeserializeState
  case object DeserializeBoolean
  case object DeserializeChain
  case object DeserializeForgingKey
  case object DeserializeDiffuse
  case object DeserializeHello
  case object DeserializeRequestBlock
  case object DeserializeRequestTine
  case object DeserializeReturnBlocks
  case object DeserializeSendBlock
  case object DeserializeSendTx
  case object DeserializeHoldersFromRemote
  case object Deserialize
}