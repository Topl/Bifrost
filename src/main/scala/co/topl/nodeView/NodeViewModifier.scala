package co.topl.nodeView

import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockSerializer}
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.network.message.InvData
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, JsonEncodable, JsonSerializable}
import co.topl.utils.{BifrostEncoder, BifrostEncoding}
import com.typesafe.config.ConfigFactory
import supertagged.TaggedType

import scala.util.Try








