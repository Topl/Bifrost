import 'dart:typed_data';

import 'package:admin/state.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:topl_common/genus/services/node_grpc.dart';
import 'package:topl_common/proto/consensus/models/block_id.pb.dart';
import 'package:topl_common/proto/consensus/models/staking.pb.dart';
import 'package:topl_common/proto/node/models/block.pb.dart';

const rpcHost = "localhost";
const rpcPort = 9084;
const maxCacheSize = 100;

void main() {
  runApp(const BifrostAdminApp());
}

class BifrostAdminApp extends StatelessWidget {
  const BifrostAdminApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Bifrost Admin',
        theme: ThemeData(
          primarySwatch: Colors.green,
        ),
        home: BifrostAdminHomePage(
          title: 'Bifrost',
          client: NodeGRPCService(host: rpcHost, port: rpcPort),
        ));
  }
}

class BifrostAdminHomePage extends StatelessWidget {
  const BifrostAdminHomePage(
      {super.key, required this.title, required this.client});

  final String title;
  final NodeGRPCService client;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: StreamBuilder(
            stream: stateStream,
            builder: (context, snapshot) => snapshot.hasData
                ? BlockchainStateViewer(state: snapshot.data!)
                : const CircularProgressIndicator()),
      ),
    );
  }

  Stream<BlockchainState> get stateStream async* {
    final genesisId = (await client.fetchBlockIdAtHeight(height: 1)).blockId;
    final genesisBlock = await fetchBlock(genesisId);
    final headId = (await client.fetchBlockIdAtDepth(depth: 0)).blockId;
    final headBlock = await fetchBlock(headId);
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
        final block = await fetchBlock(blockId);
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
            .then((r) => fetchBlock(r.blockId)));
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
        final newHead = await fetchBlock(syncTraversal.applied);
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

  Future<FullBlock> fetchBlock(BlockId blockId) async {
    final header =
        (await client.fetchBlockHeader(blockIdBytes: blockId.value)).header;
    final body =
        (await client.fetchBlockBody(blockIdBytes: blockId.value)).body;
    final transactions = [
      for (final txId in body.transactionIds)
        (await client.fetchTransaction(transactionIdBytes: txId.value))
            .transaction
    ];
    final rewardTransaction = (body.hasRewardTransactionId())
        ? (await client.fetchTransaction(
                transactionIdBytes: body.rewardTransactionId.value))
            .transaction
        : null;
    final fullBody = FullBlockBody(
        transactions: transactions, rewardTransaction: rewardTransaction);
    final fullBlock = FullBlock(header: header, fullBody: fullBody);
    return fullBlock;
  }
}

class BlockchainStateViewer extends StatelessWidget {
  final BlockchainState state;

  const BlockchainStateViewer({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Card(
          child: BlockchainHeadState(state: state),
        ),
        Card(
          child: BlockProductionRate(state: state),
        ),
        Card(
          child: StakerDistribution(state: state),
        ),
      ],
    );
  }
}

class BlockchainHeadState extends StatelessWidget {
  final BlockchainState state;

  const BlockchainHeadState({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    final head = state.blocks[state.currentHeadId]!;
    return Column(
      children: [
        const Text("Canonical Head", style: headerTextStyle),
        Row(
          children: [
            Column(
              children: [
                const Text("Block ID", style: miniHeaderTextStyle),
                Text(Base58Encode(state.currentHeadId.value))
              ],
            ),
            const VerticalDivider(),
            Column(
              children: [
                const Text("Height", style: miniHeaderTextStyle),
                Text(head.header.height.toString())
              ],
            ),
            const VerticalDivider(),
            Column(
              children: [
                const Text("Slot", style: miniHeaderTextStyle),
                Text(head.header.slot.toString())
              ],
            ),
          ],
        ),
      ],
    );
  }
}

class BlockProductionRate extends StatelessWidget {
  final BlockchainState state;

  const BlockProductionRate({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    final currentHead = state.currentHead;
    final oldestBlock = state.blocks[state.recentBlocks[0]]!;
    final recentBlocksHeightDelta =
        currentHead.header.height - oldestBlock.header.height;
    final recentBlocksMsDelta =
        currentHead.header.timestamp - oldestBlock.header.timestamp;
    final currentRate = recentBlocksHeightDelta.toDouble() /
        recentBlocksMsDelta.toDouble() *
        1000.0;

    final genesisBlock = state.genesis;
    final genesisHeightDelta =
        currentHead.header.height - genesisBlock.header.height;
    final genesisMsDelta =
        currentHead.header.timestamp - genesisBlock.header.timestamp;
    final overallRate =
        genesisHeightDelta.toDouble() / genesisMsDelta.toDouble() * 1000.0;
    return Column(
      children: [
        const Text("Block Production Rate", style: headerTextStyle),
        Row(children: [
          const Text("Recent", style: miniHeaderTextStyle),
          const VerticalDivider(),
          Text("$currentRate blocks/second")
        ]),
        Row(children: [
          const Text("Overall", style: miniHeaderTextStyle),
          const VerticalDivider(),
          Text("$overallRate blocks/second")
        ]),
      ],
    );
  }
}

class StakerDistribution extends StatelessWidget {
  final BlockchainState state;

  const StakerDistribution({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    final distributionMap = _stakeDistribution;
    final distribution = distributionMap.entries.toList()
      ..sort((e1, e2) => e1.value.compareTo(e2.value));
    final total = state.blocks.length;
    final stakerColors = Map.fromEntries(
        distribution.map((e) => MapEntry(e.key, hash32ToColor(e.key.value))));

    final pieChartData = PieChartData(
      sections: distribution
          .map((e) => PieChartSectionData(
              value: e.value.toDouble() / total.toDouble(),
              color: stakerColors[e.key],
              titleStyle: const TextStyle(color: Colors.white)))
          .toList(),
    );
    final pieChart = PieChart(pieChartData);
    final legend = Column(
      mainAxisAlignment: MainAxisAlignment.end,
      crossAxisAlignment: CrossAxisAlignment.start,
      children: distribution
          .expand((e) => [
                Indicator(
                  color: stakerColors[e.key]!,
                  text: Base58Encode(e.key.value).substring(0, 8),
                  isSquare: true,
                ),
                const SizedBox(
                  height: 4,
                ),
              ])
          .toList(),
    );
    return Column(
      children: [
        const Text("Staker Block Distribution", style: headerTextStyle),
        SizedBox(
          height: 500,
          child: AspectRatio(
            aspectRatio: 1.3,
            child: Row(
              children: [
                const SizedBox(height: 18),
                Expanded(child: pieChart),
                legend,
                const SizedBox(height: 18),
              ],
            ),
          ),
        ),
      ],
    );
  }

  StakeDistribution get _stakeDistribution {
    final distribution = <StakingAddress, int>{};
    for (final block in state.blocks.values) {
      final address = block.header.address;
      distribution[address] = (distribution[address] ?? 0) + 1;
    }
    return distribution;
  }
}

typedef StakeDistribution = Map<StakingAddress, int>;

const miniHeaderTextStyle =
    TextStyle(fontWeight: FontWeight.bold, fontSize: 15);
const headerTextStyle = TextStyle(fontWeight: FontWeight.bold, fontSize: 18);

Color hash32ToColor(List<int> bytes) {
  final buffered = ByteData.view(Uint8List.fromList(bytes).buffer);
  final r = buffered.getUint8(0);
  final g = buffered.getUint8(1);
  final b = buffered.getUint8(2);
  return Color.fromRGBO(r, g, b, 1);
}

class Indicator extends StatelessWidget {
  const Indicator({
    super.key,
    required this.color,
    required this.text,
    required this.isSquare,
    this.size = 16,
    this.textColor,
  });
  final Color color;
  final String text;
  final bool isSquare;
  final double size;
  final Color? textColor;

  @override
  Widget build(BuildContext context) {
    return Row(
      children: <Widget>[
        Container(
          width: size,
          height: size,
          decoration: BoxDecoration(
            shape: isSquare ? BoxShape.rectangle : BoxShape.circle,
            color: color,
          ),
        ),
        const SizedBox(
          width: 4,
        ),
        Text(
          text,
          style: TextStyle(
            fontSize: 16,
            fontWeight: FontWeight.bold,
            color: textColor,
          ),
        )
      ],
    );
  }
}
