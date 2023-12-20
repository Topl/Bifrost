
import 'package:grpc/grpc.dart';
import 'package:topl_common/genus/services/node_grpc.dart';
import 'package:topl_common/proto/consensus/models/block_id.pb.dart';
import 'package:topl_common/proto/node/models/block.pb.dart';

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

  Future<Duration> latency() async {
    final start = DateTime.now();
    await fetchBlockIdAtHeight(height: 1);
    final end = DateTime.now();
    return Duration(
        milliseconds:
            end.millisecondsSinceEpoch - start.millisecondsSinceEpoch);
  }

  Future<Duration> averageLatency({int attempts = 5}) async {
    int totalMs = 0;
    for (int i = 0; i < attempts; i++) {
      totalMs += (await latency()).inMilliseconds;
    }
    return Duration(milliseconds: totalMs ~/ attempts);
  }
}
