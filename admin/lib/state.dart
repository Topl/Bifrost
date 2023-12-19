import 'dart:io';

import 'package:fixnum/fixnum.dart';
import 'package:topl_common/genus/services/node_grpc.dart';
import 'package:topl_common/proto/consensus/models/block_id.pb.dart';
import 'package:topl_common/proto/node/models/block.pb.dart';
import 'package:grpc/grpc.dart';

class BlockchainState {
  Map<BlockId, FullBlock> blocks;
  List<BlockId> recentBlocks;
  final FullBlock genesis;
  final int maxSize;

  BlockchainState({
    required this.blocks,
    required this.recentBlocks,
    required this.genesis,
    required this.maxSize,
  });

  factory BlockchainState.fromCanonicalHead({
    required FullBlock head,
    required FullBlock genesis,
    required int maxSize,
  }) {
    return BlockchainState(
      blocks: {head.header.headerId: head},
      recentBlocks: [head.header.headerId],
      genesis: genesis,
      maxSize: maxSize,
    );
  }

  static Stream<BlockchainState> streamed(
      NodeGRPCService client, int maxCacheSize) async* {
    final genesisId = (await client.fetchBlockIdAtHeight(height: 1)).blockId;
    final genesisBlock = await client.fetchBlock(genesisId);
    final headId = (await client.fetchBlockIdAtDepth(depth: 0)).blockId;
    final headBlock = await client.fetchBlock(headId);
    BlockchainState state;
    if (genesisId != headId) {
      Int64 h = headBlock.header.height - maxCacheSize;
      if (h < genesisBlock.header.height) {
        h = genesisBlock.header.height;
        state = BlockchainState.fromCanonicalHead(
            head: genesisBlock, genesis: genesisBlock, maxSize: maxCacheSize);
      } else {
        final blockId =
            (await client.fetchBlockIdAtHeight(height: h.toInt())).blockId;
        final block = await client.fetchBlock(blockId);
        state = BlockchainState.fromCanonicalHead(
            head: block, genesis: genesisBlock, maxSize: maxCacheSize);
      }

      h++;
      final futureBlocks = <Future<FullBlock>>[];
      for (int height = h.toInt();
          height < headBlock.header.height.toInt();
          height++) {
        futureBlocks.add(client
            .fetchBlockIdAtHeight(height: height)
            .then((r) => client.fetchBlock(r.blockId)));
      }
      final blocks = await Future.wait(futureBlocks);
      for (final block in blocks) {
        state.pushBlock(block);
      }
      state.pushBlock(headBlock);
    } else {
      state = BlockchainState.fromCanonicalHead(
          head: headBlock, genesis: genesisBlock, maxSize: maxCacheSize);
    }
    yield state;
    await for (final syncTraversal in client.synchronizationTraversal()) {
      if (syncTraversal.hasApplied()) {
        final newHead = await client.fetchBlock(syncTraversal.applied);
        state.pushBlock(newHead);
        yield state;
      } else {
        if (!state.isEmpty) {
          state.popBlock();
          yield state;
        }
      }
    }
  }

  BlockId get currentHeadId => recentBlocks[recentBlocks.length - 1];

  void pushBlock(FullBlock block) {
    blocks[block.header.headerId] = block;
    recentBlocks.add(block.header.headerId);
    if (recentBlocks.length > maxSize) {
      final oldest = recentBlocks.removeAt(0);
      blocks.remove(oldest);
    }
  }

  FullBlock popBlock() {
    final result = blocks[currentHeadId]!;
    blocks.remove(currentHeadId);
    recentBlocks.removeLast();
    return result;
  }

  bool get isEmpty => blocks.isEmpty;

  FullBlock get currentHead => blocks[currentHeadId]!;
}

extension NodeGRPCServiceOps on NodeGRPCService {
  Future<void> whenReady() async {
    bool ready = false;
    while (!ready) {
      try {
        await fetchBlockIdAtHeight(
            height: 1,
            options: CallOptions(timeout: const Duration(seconds: 5)));
        ready = true;
      } on Exception {
        await Future.delayed(const Duration(seconds: 2));
      }
    }
  }

  Future<FullBlock> fetchBlock(BlockId blockId) async {
    final header = (await fetchBlockHeader(blockIdBytes: blockId.value)).header;
    final body = (await fetchBlockBody(blockIdBytes: blockId.value)).body;
    final transactions = [
      for (final txId in body.transactionIds)
        (await fetchTransaction(transactionIdBytes: txId.value)).transaction
    ];
    final rewardTransaction = (body.hasRewardTransactionId())
        ? (await fetchTransaction(
                transactionIdBytes: body.rewardTransactionId.value))
            .transaction
        : null;
    final fullBody = FullBlockBody(
        transactions: transactions, rewardTransaction: rewardTransaction);
    final fullBlock = FullBlock(header: header, fullBody: fullBody);
    return fullBlock;
  }
}
