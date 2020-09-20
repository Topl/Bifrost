package bifrost.state

import bifrost.modifier.ModifierId
import bifrost.modifier.box.Box
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

trait Registry extends Logging {

  val store: LSMStore

  def getBox(id: ModifierId, utxoStore: LSMStore): Option[Box] =
    utxoStore.get(ByteArrayWrapper(id.hashBytes))
      .map(_.data)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def update( newVersion: VersionTag,
              keyFilteredBoxIdsToRemove: Set[ModifierId],
              keyFilteredBoxesToAdd: Set[Box]): Try[Registry]

  def rollbackTo( version: VersionTag,
                  utxoStore: LSMStore): Try[Registry]

}
