import 'dart:math';
import 'dart:typed_data';

import 'package:admin/state.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:syncfusion_flutter_gauges/gauges.dart';
import 'package:topl_common/genus/services/node_grpc.dart';
import 'package:topl_common/proto/consensus/models/staking.pb.dart';

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
        home: const BifrostAdminHomePage(
          title: 'Bifrost Admin',
        ));
  }
}

class BifrostAdminHomePage extends StatefulWidget {
  const BifrostAdminHomePage({super.key, required this.title});

  final String title;

  @override
  State<BifrostAdminHomePage> createState() => _BifrostAdminHomePageState();
}

class _BifrostAdminHomePageState extends State<BifrostAdminHomePage> {
  String currentAddress = "";
  String _addressBuffer = "$rpcHost:$rpcPort";

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.title)),
      body: currentAddress.isEmpty ? _waitingForInput : _ready,
    );
  }

  Widget get _waitingForInput => Center(child: _addressBar);

  Widget get _ready {
    final split = currentAddress.split(":");
    final client = NodeGRPCService(host: split[0], port: int.parse(split[1]));
    return SingleChildScrollView(
      child: Column(
        children: [
          _addressBar,
          StreamBuilder(
            stream: Stream.fromFuture(client.whenReady()).asyncExpand(
                (_) => BlockchainState.streamed(client, maxCacheSize)),
            builder: (context, snapshot) => snapshot.hasData
                ? BlockchainStateViewer(state: snapshot.data!)
                : const CircularProgressIndicator(),
          ),
        ],
      ),
    );
  }

  Widget get _addressBar => SizedBox(
        height: 100,
        child: Row(
          children: [
            SizedBox(
              width: 300,
              child: TextFormField(
                decoration: const InputDecoration(
                    hintText: "host:port", border: OutlineInputBorder()),
                initialValue: _addressBuffer,
                onChanged: (text) => _addressBuffer = text,
              ),
            ),
            IconButton(
              onPressed: () => setState(() {
                currentAddress = _addressBuffer;
              }),
              icon: const Icon(Icons.send),
            )
          ],
        ),
      );
}

class BlockchainStateViewer extends StatelessWidget {
  final BlockchainState state;

  const BlockchainStateViewer({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Card(child: BlockchainHeadState(state: state)),
        Card(child: BlockProductionRate(state: state)),
        Card(child: StakerDistribution(state: state)),
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
    return Container(
      color: hash32ToLightColor(state.currentHeadId.value),
      child: Column(
        children: [
          const Text("Canonical Head", style: headerTextStyle),
          Wrap(
            children: [
              _dataChip("Block ID", Base58Encode(state.currentHeadId.value)),
              const VerticalDivider(),
              _dataChip("Height", head.header.height.toString()),
              const VerticalDivider(),
              _dataChip("Slot", head.header.slot.toString()),
            ],
          ),
        ],
      ),
    );
  }

  Widget _dataChip(String header, String value) => Column(
        children: [
          Text(header, style: miniHeaderTextStyle),
          Chip(label: Text(value))
        ],
      );
}

class BlockProductionRate extends StatelessWidget {
  final BlockchainState state;

  const BlockProductionRate({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    return Column(children: [
      const Text("Block Production Rate", style: headerTextStyle),
      Wrap(
        children: [
          _guageWrapper(
              "Last 10 Blocks",
              Stream.periodic(const Duration(milliseconds: 500))
                  .map((_) => _currentProductionRate)),
          _guageWrapper("Last ${state.recentBlocks.length} Blocks",
              Stream.value(_cachedProductionRate)),
          _guageWrapper("Overall", Stream.value(_overallProductionRate)),
        ],
      )
    ]);
  }

  Widget _guageWrapper(String title, Stream<double> rates) => SizedBox(
        width: 400,
        height: 400,
        child: Column(children: [
          Text(title, style: miniHeaderTextStyle),
          const Divider(),
          StreamBuilder(
              stream: rates,
              builder: (_, snapshot) =>
                  _productionRateGauge(snapshot.data ?? 0))
        ]),
      );

  Widget _productionRateGauge(double rate) {
    return SfRadialGauge(axes: <RadialAxis>[
      RadialAxis(
          minimum: 0,
          maximum: 0.35,
          ranges: _gaugeRanges,
          pointers: <GaugePointer>[
            NeedlePointer(value: rate)
          ],
          annotations: <GaugeAnnotation>[
            GaugeAnnotation(
              widget: Column(
                children: [
                  Text(rate.toStringAsPrecision(5),
                      style: const TextStyle(
                          fontSize: 14, fontWeight: FontWeight.bold)),
                  const Text("blocks-per-second",
                      style: TextStyle(fontSize: 10)),
                ],
              ),
              angle: rate,
              positionFactor: 0.5,
              verticalAlignment: GaugeAlignment.near,
            )
          ])
    ]);
  }

  static final _gaugeRanges = <GaugeRange>[
    GaugeRange(startValue: 0, endValue: 0.06, color: Colors.red),
    GaugeRange(startValue: 0.06, endValue: 0.09, color: Colors.yellow),
    GaugeRange(startValue: 0.09, endValue: 0.2, color: Colors.green),
    GaugeRange(startValue: 0.2, endValue: 0.28, color: Colors.yellow),
    GaugeRange(startValue: 0.28, endValue: 0.35, color: Colors.red)
  ];

  double get _currentProductionRate {
    final currentHead = state.currentHead;
    final targetBlock = state
        .blocks[state.recentBlocks[max(state.recentBlocks.length - 10, 0)]]!;
    final recentBlocksHeightDelta =
        currentHead.header.height - targetBlock.header.height;
    final recentBlocksMsDelta = DateTime.now().millisecondsSinceEpoch -
        targetBlock.header.timestamp.toInt();
    final currentRate = recentBlocksHeightDelta.toDouble() /
        recentBlocksMsDelta.toDouble() *
        1000.0;
    return currentRate;
  }

  double get _cachedProductionRate {
    final currentHead = state.currentHead;
    final oldestBlock = state.blocks[state.recentBlocks[0]]!;
    final recentBlocksHeightDelta =
        currentHead.header.height - oldestBlock.header.height;
    final recentBlocksMsDelta = DateTime.now().millisecondsSinceEpoch -
        oldestBlock.header.timestamp.toInt();
    final currentRate = recentBlocksHeightDelta.toDouble() /
        recentBlocksMsDelta.toDouble() *
        1000.0;
    return currentRate;
  }

  double get _overallProductionRate {
    final currentHead = state.currentHead;
    final genesisBlock = state.genesis;
    final genesisHeightDelta =
        currentHead.header.height - genesisBlock.header.height;
    final genesisMsDelta = DateTime.now().millisecondsSinceEpoch -
        genesisBlock.header.timestamp.toInt();
    final overallRate =
        genesisHeightDelta.toDouble() / genesisMsDelta.toDouble() * 1000.0;
    return overallRate;
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
    final stakerColors = Map.fromEntries(distribution
        .map((e) => MapEntry(e.key, hash32ToLightColor(e.key.value))));

    final pieChartData = PieChartData(
      sections: distribution
          .map((e) => PieChartSectionData(
                value: e.value.toDouble(),
                color: stakerColors[e.key],
                title:
                    "${e.value} (${(e.value.toDouble() / total.toDouble() * 100.0).round()}%)",
                titleStyle: const TextStyle(color: Colors.black),
              ))
          .toList(),
      borderData: FlBorderData(),
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
        Text(
            "Staker Block Distribution (Last ${state.recentBlocks.length} Blocks)",
            style: headerTextStyle),
        ConstrainedBox(
          constraints: BoxConstraints.loose(const Size.fromHeight(400)),
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

Color hash32ToLightColor(List<int> bytes) {
  final buffered = ByteData.view(Uint8List.fromList(bytes).buffer);
  final intMaxValue = Int32.MAX_VALUE.toInt() - Int32.MIN_VALUE.toInt();
  final hue = buffered.getUint32(4).toDouble() * 360.0 / intMaxValue.toDouble();
  return HSLColor.fromAHSL(1, hue, 0.8, 0.8).toColor();
}

Color hash32ToDarkColor(List<int> bytes) {
  final buffered = ByteData.view(Uint8List.fromList(bytes).buffer);
  final intMaxValue = Int32.MAX_VALUE.toInt() - Int32.MIN_VALUE.toInt();
  final hue = buffered.getUint32(4).toDouble() * 360.0 / intMaxValue.toDouble();
  return HSLColor.fromAHSL(1, hue, 0.8, 0.2).toColor();
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
            fontSize: 13,
            fontWeight: FontWeight.bold,
            color: textColor,
          ),
        )
      ],
    );
  }
}
