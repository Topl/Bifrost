import 'dart:math';
import 'package:admin/constants.dart';
import 'package:admin/state.dart';
import 'package:admin/ui/utils.dart';
import 'package:flutter/material.dart';
import 'package:syncfusion_flutter_gauges/gauges.dart';
import 'package:topl_common/genus/data_extensions.dart';

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
          maximum: _targetSecondsPerBlock * 2,
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
                  const Text("seconds-per-block",
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

  double get _targetBlocksPerSecond {
    final fEffective = state.protocol.fEffective.numerator.toBigInt() /
        state.protocol.fEffective.denominator.toBigInt();
    return fEffective + (networkDelay.inMilliseconds / 1000 * fEffective);
  }

  double get _targetSecondsPerBlock => 1.0 / _targetBlocksPerSecond;

  List<GaugeRange> get _gaugeRanges {
    final targetSecondsPerBlock = _targetSecondsPerBlock;

    return <GaugeRange>[
      GaugeRange(
          startValue: 0,
          endValue: targetSecondsPerBlock * 0.4,
          color: Colors.red),
      GaugeRange(
          startValue: targetSecondsPerBlock * 0.4,
          endValue: targetSecondsPerBlock * 0.8,
          color: Colors.yellow),
      GaugeRange(
          startValue: targetSecondsPerBlock * 0.8,
          endValue: targetSecondsPerBlock * 1.2,
          color: Colors.green),
      GaugeRange(
          startValue: targetSecondsPerBlock * 1.2,
          endValue: targetSecondsPerBlock * 1.6,
          color: Colors.yellow),
      GaugeRange(
          startValue: targetSecondsPerBlock * 1.6,
          endValue: targetSecondsPerBlock * 2,
          color: Colors.red)
    ];
  }

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
    return 1.0 / currentRate;
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
    return 1.0 / currentRate;
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
    return 1.0 / overallRate;
  }
}
