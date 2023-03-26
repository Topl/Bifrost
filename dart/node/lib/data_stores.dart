import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/interpreters/in_memory_store.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fpdart/fpdart.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:bifrost_common/algebras/store_algebra.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class DataStores {
  final StoreAlgebra<BlockId, Tuple2<Int64, BlockId>> parentChildTree;
  final StoreAlgebra<int, BlockId> currentEventIds;
  final StoreAlgebra<BlockId, SlotData> slotData;
  final StoreAlgebra<BlockId, BlockHeader> headers;
  final StoreAlgebra<BlockId, BlockBody> bodies;
  final StoreAlgebra<Identifier_IoTransaction32, IoTransaction> transactions;
  final StoreAlgebra<Identifier_IoTransaction32, Set<int>> spendableBoxIds;
  final StoreAlgebra<Int64, BlockId> epochBoundaries;
  final StoreAlgebra<StakingAddress, BigInt> operatorStakes;
  final StoreAlgebra<void, BigInt> activeStake;
  final StoreAlgebra<StakingAddress, SignatureKesProduct> registrations;
  final StoreAlgebra<Int64, BlockId> blockHeightTree;

  DataStores({
    required this.parentChildTree,
    required this.currentEventIds,
    required this.slotData,
    required this.headers,
    required this.bodies,
    required this.transactions,
    required this.spendableBoxIds,
    required this.epochBoundaries,
    required this.operatorStakes,
    required this.activeStake,
    required this.registrations,
    required this.blockHeightTree,
  });

  static Future<DataStores> init(FullBlock genesisBlock) async {
    makeDb<Key, Value>() => InMemoryStore<Key, Value>();

    final stores = DataStores(
      parentChildTree: makeDb(),
      currentEventIds: makeDb(),
      slotData: makeDb(),
      headers: makeDb(),
      bodies: makeDb(),
      transactions: makeDb(),
      spendableBoxIds: makeDb(),
      epochBoundaries: makeDb(),
      operatorStakes: makeDb(),
      activeStake: makeDb(),
      registrations: makeDb(),
      blockHeightTree: makeDb(),
    );

    final genesisBlockId = genesisBlock.header.id;

    await stores.currentEventIds
        .put(CurreventEventIdGetterSetterIndices.CanonicalHead, genesisBlockId);
    for (final key in [
      CurreventEventIdGetterSetterIndices.ConsensusData,
      CurreventEventIdGetterSetterIndices.EpochBoundaries,
      CurreventEventIdGetterSetterIndices.BlockHeightTree,
      CurreventEventIdGetterSetterIndices.BoxState,
      CurreventEventIdGetterSetterIndices.Mempool,
    ]) {
      await stores.currentEventIds.put(key, genesisBlock.header.parentHeaderId);
    }

    await stores.slotData.put(genesisBlockId, genesisBlock.header.slotData);
    await stores.headers.put(genesisBlockId, genesisBlock.header);
    await stores.bodies.put(
      genesisBlockId,
      BlockBody(
          transactionIds: genesisBlock.fullBody.transaction.map((t) => t.id)),
    );
    for (final transaction in genesisBlock.fullBody.transaction) {
      await stores.transactions.put(transaction.id, transaction);
    }
    await stores.blockHeightTree
        .put(Int64(0), genesisBlock.header.parentHeaderId);
    if (!await stores.activeStake.contains({})) {
      await stores.activeStake.put({}, BigInt.from(0));
    }
    return stores;
  }
}

class CurreventEventIdGetterSetterIndices {
  static const CanonicalHead = 0;
  static const ConsensusData = 1;
  static const EpochBoundaries = 2;
  static const BlockHeightTree = 3;
  static const BoxState = 4;
  static const Mempool = 5;
}

class CurrentEventIdGetterSetters {
  final StoreAlgebra<int, BlockId> store;

  CurrentEventIdGetterSetters(this.store);

  GetterSetter get canonicalHead => GetterSetter.forByte(
      store, CurreventEventIdGetterSetterIndices.CanonicalHead);

  GetterSetter get consensusData => GetterSetter.forByte(
      store, CurreventEventIdGetterSetterIndices.ConsensusData);

  GetterSetter get epochBoundaries => GetterSetter.forByte(
      store, CurreventEventIdGetterSetterIndices.EpochBoundaries);

  GetterSetter get blockHeightTree => GetterSetter.forByte(
      store, CurreventEventIdGetterSetterIndices.BlockHeightTree);

  GetterSetter get boxState =>
      GetterSetter.forByte(store, CurreventEventIdGetterSetterIndices.BoxState);

  GetterSetter get mempool =>
      GetterSetter.forByte(store, CurreventEventIdGetterSetterIndices.Mempool);
}

class GetterSetter {
  final Future<BlockId> Function() get;
  final Future<void> Function(BlockId) set;

  GetterSetter(this.get, this.set);

  factory GetterSetter.forByte(StoreAlgebra<int, BlockId> store, int byte) =>
      GetterSetter(
          () => store.getOrRaise(byte), (value) => store.put(byte, value));
}
