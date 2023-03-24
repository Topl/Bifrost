import 'dart:async';
import 'dart:math';

import 'package:args/args.dart';
import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_consensus/interpreters/ChainSelection.dart';
import 'package:bifrost_consensus/interpreters/LocalChain.dart';
import 'package:bifrost_grpc/interpreters/ToplGrpcService.dart';
import 'package:bifrost_minting/interpreters/BlockProducer.dart';
import 'package:bifrost_minting/interpreters/BlockPacker.dart';
import 'package:bifrost_common/interpreters/Clock.dart';
import 'package:bifrost_common/interpreters/InMemoryStore.dart';
import 'package:bifrost_minting/interpreters/LeaderElectionMinting.dart';
import 'package:bifrost_minting/interpreters/OperationalKeys.dart';
import 'package:bifrost_minting/interpreters/Staker.dart';
import 'package:bifrost_minting/interpreters/VrfProofs.dart';
import 'package:bifrost_node/PeerConsumer.dart';
import 'package:bifrost_p2p/interpreters/P2PServer.dart';
import 'package:fixnum/fixnum.dart';
import 'package:async/async.dart' show StreamGroup;
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

void main(List<String> args) async {
  print("Command args=$args");

  final appArgs = argParser.parse(args);
  print(
      "Parsed args: ${appArgs.options.map((n) => "$n=${appArgs[n]}").join(" ")}");

  final bigBangSlotData = SlotData(
      parentSlotId: SlotId(slot: Int64(-1)),
      slotId: SlotId(slot: Int64(0)),
      height: Int64(1));

  final operatorAddress = StakingAddress();

  final vrfVK = List.filled(32, 0);

  final clock = Clock(
    Duration(milliseconds: 500),
    ChainSelectionKLookback * 6,
    Int64(DateTime.now().millisecondsSinceEpoch),
    Int64(50),
  );

  final vrfProofs = VrfProofs(clock);

  final leaderElectionMinting = LeaderElectionMinting(vrfVK, vrfProofs);

  final operationalKeys = OperationalKeys();

  final slotDataStore = InMemoryStore<BlockId, SlotData>();

  final headerStore = InMemoryStore<BlockId, BlockHeader>();

  final bodyStore = InMemoryStore<BlockId, BlockBody>();

  final transactionStore = InMemoryStore<Identifier_IoTransaction32, IoTransaction>();

  final localChain = LocalChain(bigBangSlotData.slotId.blockId);

  final chainSelection = ChainSelection(slotDataStore.getOrRaise);

  final validateFullBlock = (FullBlock block) async {
    // TODO
    final parentHeader =
        await headerStore.getOrRaise(block.header.parentHeaderId);
    if (block.header.slot <= parentHeader.slot) return "Non-forward slot";
  };

  final peerConsumer = PeerConsumer(
    slotDataStore,
    headerStore,
    bodyStore,
    transactionStore,
    localChain,
    chainSelection,
    validateFullBlock,
  );

  final staker = Staker(
    operatorAddress,
    leaderElectionMinting,
    operationalKeys,
    vrfProofs,
    clock,
  );

  final knownPeers = (appArgs["p2p-known-peers"] as String)
      .split(',')
      .where((s) => s.isNotEmpty);

  final blockProducer = BlockProducer(
    StreamGroup.merge([
      Stream.value(bigBangSlotData),
      localChain.adoptions.asyncMap(slotDataStore.getOrRaise),
    ]),
    staker,
    clock,
    BlockPacker(),
  );
  final p2pId = Random().nextInt(1000000).toString();
  print("p2pId=$p2pId");
  final rpcServer = ToplGrpcService(
    p2pId,
    slotDataStore.get,
    headerStore.get,
    bodyStore.get,
    transactionStore.get,
    () => localChain.adoptions,
  );
  final p2pServer = P2PServer(
    p2pId,
    appArgs["p2p-bind-host"],
    int.parse(appArgs["p2p-bind-port"]),
    rpcServer,
    Stream.fromFuture(Future.delayed(Duration(seconds: 5)))
        .asyncExpand((_) => Stream.fromIterable(knownPeers)),
  );
  await p2pServer.start();

  final mintedBlocksStream = await blockProducer.blocks;

  unawaited(
    mintedBlocksStream.asyncMap((b) async {
      final id = b.header.id;
      final slotData = SlotData(
        slotId: SlotId(slot: b.header.slot),
        parentSlotId:
            SlotId(slot: b.header.parentSlot, blockId: b.header.parentHeaderId),
        height: b.header.height,
      );
      final body =
          BlockBody(transactionIds: b.fullBody.transaction.map((t) => t.id));

      await slotDataStore.put(id, slotData);
      await headerStore.put(id, b.header);
      await bodyStore.put(id, body);
      await localChain.adopt(id);
      return b;
    }).forEach((block) {
      print(
          "Minted block. id=${block.header.id.show} height=${block.header.height} slot=${block.header.slot}");
    }),
  );

  unawaited(p2pServer.clients
      .forEach((client) => unawaited(peerConsumer.consume(client).last)));
}

T _unimplemented<T>() {
  throw new UnimplementedError();
}

final ChainSelectionKLookback = Int64(50);

final argParser = ArgParser()
  ..addOption("p2p-bind-host", defaultsTo: "0.0.0.0")
  ..addOption("p2p-bind-port", defaultsTo: "9084")
  ..addOption("p2p-known-peers", defaultsTo: "");
