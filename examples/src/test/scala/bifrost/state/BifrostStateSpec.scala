package bifrost.state

import java.io.File
import java.time.Instant

import bifrost.BifrostGenerators
import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.state.BifrostState
import examples.bifrost.transaction.ContractCreation
import examples.bifrost.transaction.box.{ContractBox, StableCoinBox}
import examples.bifrost.transaction.box.proposition.MofNPropositionSerializer
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.Try

class BifrostStateSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {

  property("A block with valid contract creation will result in an entry in the LSMStore") {

    val dir = new File("dataDir")

    dir.mkdirs()

    val store = new LSMStore(dir)
    store.update(
      ByteArrayWrapper(Ints.toByteArray(0)),
      Seq(),
      Seq(ByteArrayWrapper(
        FastCryptographicHash("timestamp".getBytes)) -> ByteArrayWrapper(Longs.toByteArray(Instant.now().toEpochMilli))
      )
    )
    var genesisState = BifrostState(store, Ints.toByteArray(0), Instant.now().toEpochMilli)

    genesisState = genesisState

    // Create block with contract creation
    forAll(contractCreationGen) {
      cc: ContractCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now().toEpochMilli,
          StableCoinBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val boxType = "ContractBox"

        val box = cc.newBoxes.head.asInstanceOf[ContractBox]

        val boxBytes = Ints.toByteArray(boxType.getBytes.length) ++
          boxType.getBytes ++
          MofNPropositionSerializer.toBytes(box.proposition) ++
          Longs.toByteArray(box.nonce) ++
          Ints.toByteArray(box.value.getBytes.length) ++
          box.value.getBytes


        val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(1)).get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        genesisState = newState.rollbackTo(Ints.toByteArray(0)).get

    }

    store.close()
    val path: Path = Path ("dataDir")
    Try(path.deleteRecursively())
  }

  property("A block with valid stablecoin transfer should result in more funds for receiver, less for transferrer") {
    // Create genesis block, add to state
    // Create new block with stablecoin transfer
    // send new block to state
    // check updated state
  }

  property("Attempting to validate a contract creation tx without valid signatures should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a contract creation tx with a timestamp that is before the last block timestamp should error") {

  }

  property("Attempting to validate a contract creation tx with a timestamp that is in the future should error") {

  }

  property("Attempting to validate a contract creation tx with the same signature as an existing contract should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a contract creation tx with a timestamp too far in the future should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a stablecointransfer for amount you do not have should error") {
    // Create invalid stablecointransfer
    // send tx to state
  }

  property("Attempting to validate a stablecointransfer without valid signature should error") {
    // Create invalid stablecointransfer
    // send tx to state
  }

}