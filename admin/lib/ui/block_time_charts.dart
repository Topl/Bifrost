import 'dart:math';

import 'package:admin/state.dart';
import 'package:admin/ui/utils.dart';
import 'package:flutter/material.dart';
import 'package:syncfusion_flutter_charts/sparkcharts.dart';
import 'package:topl_common/proto/consensus/models/block_id.pb.dart';

class BlockTimeCharts extends StatelessWidget {
  final BlockchainState state;

  const BlockTimeCharts({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    return Wrap(
      children: [
        Padding(
          padding: const EdgeInsets.all(8.0),
          child: _slotGapChart,
        ),
        Padding(
          padding: const EdgeInsets.all(8.0),
          child: _networkDelayChart,
        ),
      ],
    );
  }

  Widget get _slotGapChart => SizedBox(
        width: 400,
        height: 400,
        child: Column(children: [
          const Text("Production Delay (ms)", style: headerTextStyle),
          Wrap(
            children: [
              SfSparkLineChart(
                data: _slotGapData,
                labelDisplayMode: SparkChartLabelDisplayMode.all,
                marker: const SparkChartMarker(
                  displayMode: SparkChartMarkerDisplayMode.all,
                ),
              ),
            ],
          )
        ]),
      );

  Widget get _networkDelayChart => SizedBox(
        width: 400,
        height: 400,
        child: Column(children: [
          const Text("Network Delay (ms)", style: headerTextStyle),
          Wrap(
            children: [
              SfSparkLineChart(
                data: _networkDelayData,
                labelDisplayMode: SparkChartLabelDisplayMode.all,
                marker: const SparkChartMarker(
                  displayMode: SparkChartMarkerDisplayMode.all,
                ),
              ),
            ],
          )
        ]),
      );

  List<double> get _slotGapData {
    if (state.recentBlocks.length < 2) return [];

    final List<BlockId> blockIds;
    if (state.recentBlocks.length > 10) {
      blockIds = state.recentBlocks.sublist(state.recentBlocks.length - 11);
    } else {
      blockIds = state.recentBlocks;
    }
    final List<double> delays = [];
    for (int i = 1; i < blockIds.length; i++) {
      final delay = state.blocks[blockIds[i]]!.header.timestamp -
          state.blocks[blockIds[i - 1]]!.header.timestamp;
      delays.add(delay.toDouble());
    }
    return delays;
  }

  List<double> get _networkDelayData {
    List<double> data = [];
    for (int i = max(state.recentAdoptionTimestamps.length - 10, 0);
        i < state.recentAdoptionTimestamps.length;
        i++) {
      final adoptionTimestamp = state.recentAdoptionTimestamps[i];
      if (adoptionTimestamp != null) {
        final timestamp = state.blocks[state.recentBlocks[i]]!.header.timestamp;
        final delay = adoptionTimestamp.millisecondsSinceEpoch -
            timestamp.toInt() -
            state.baselineRpcLatency.inMilliseconds;
        data.add(delay.toDouble());
      }
    }
    return data;
  }
}
