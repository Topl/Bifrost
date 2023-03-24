import 'dart:async';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_consensus/algebras/ChainSelectionAlgebra.dart';
import 'package:bifrost_consensus/algebras/LocalChainAlgebra.dart';

import 'package:bifrost_common/algebras/StoreAlgebra.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';
import 'package:topl_protobuf/node/services/bifrost_rpc.pb.dart';
import 'package:topl_protobuf/node/services/bifrost_rpc.pbgrpc.dart';

class PeerConsumer {
  final StoreAlgebra<BlockId, SlotData> slotDataStore;
  final StoreAlgebra<BlockId, BlockHeader> blockHeaderStore;
  final StoreAlgebra<BlockId, BlockBody> blockBodyStore;
  final StoreAlgebra<Identifier_IoTransaction32, IoTransaction>
      transactionStore;
  final LocalChainAlgebra localChain;
  final ChainSelectionAlgebra chainSelection;
  final FutureOr<String?> Function(FullBlock) validateFullBlock;

  PeerConsumer(
    this.slotDataStore,
    this.blockHeaderStore,
    this.blockBodyStore,
    this.transactionStore,
    this.localChain,
    this.chainSelection,
    this.validateFullBlock,
  );

  Stream<PeerEvent> consume(NodeRpcClient peer) {
    return peer
        .synchronizationTraversal(SynchronizationTraversalReq())
        .expand<BlockId>((t) => t.hasApplied() ? [t.applied] : [])
        .asyncMap(
      (blockId) async {
        print("Fetching tine from remote head=${blockId.show}");
        final tine = await _fetchTine(peer, blockId);
        tine.forEach((slotData) async =>
            await slotDataStore.put(slotData.slotId.blockId, slotData));
        final currentHead = await localChain.currentHead;
        final bestHead = await chainSelection.select(currentHead, blockId);
        if (blockId == bestHead) {
          await Stream.fromIterable(tine).asyncMap((slotData) async {
            final id = slotData.slotId.blockId;
            print("Fetching remote header id=${id.show}");
            final header =
                (await peer.fetchBlockHeader(FetchBlockHeaderReq(blockId: id)))
                    .header;
            print("Fetching remote body id=${id.show}");
            final body =
                (await peer.fetchBlockBody(FetchBlockBodyReq(blockId: id)))
                    .body;
            print("Fetching remote transactions for block id=${id.show}");
            final transactions = await Stream.fromIterable(body.transactionIds)
                .asyncMap((transactionId) async =>
                    (await transactionStore.get(transactionId)) ??
                    (await peer.fetchTransaction(
                            FetchTransactionReq(transactionId: transactionId)))
                        .transaction)
                .toList();
            final fullBlock = FullBlock(
                header: header,
                fullBody: FullBlockBody(transaction: transactions));
            print("Validating full block id=${id.show}");
            final maybeValidationError = await validateFullBlock(fullBlock);
            if (maybeValidationError != null)
              throw ArgumentError(
                  "Invalid block.  reason=$maybeValidationError");
            print("Saving block id=${id.show}");
            await blockHeaderStore.put(id, header);
            await blockBodyStore.put(id, body);
            for (int i = 0; i < transactions.length; i++) {
              final transactionId = body.transactionIds[i];
              if (!(await transactionStore.contains(transactionId))) {
                await transactionStore.put(transactionId, transactions[i]);
              }
            }
            return fullBlock;
          }).forEach((_) {});

          print("Adopting block id=${blockId.show}");
          await localChain.adopt(blockId);
        }
      },
    ).expand((_) => <PeerEvent>[]);
  }

  Future<List<SlotData>> _fetchTine(
      NodeRpcClient peer, BlockId remoteBlockId) async {
    if (await slotDataStore.contains(remoteBlockId)) return [];

    List<SlotData> tine = [
      (await peer.fetchSlotData(FetchSlotDataReq(blockId: remoteBlockId)))
          .slotData
    ];

    while (!await slotDataStore.contains(tine.first.parentSlotId.blockId)) {
      tine.insert(
        0,
        (await peer.fetchSlotData(
                FetchSlotDataReq(blockId: tine.first.parentSlotId.blockId)))
            .slotData,
      );
    }
    return tine;
  }
}

abstract class PeerEvent {}
