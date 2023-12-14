import 'package:topl_common/proto/consensus/models/block_id.pb.dart';
import 'package:topl_common/proto/node/models/block.pb.dart';

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
