import 'package:admin/state.dart';
import 'package:admin/ui/utils.dart';
import 'package:fixnum/fixnum.dart';
import 'package:flutter/material.dart';

class BlockchainTimeInfo extends StatelessWidget {
  final BlockchainState state;

  const BlockchainTimeInfo({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    final networkDelay = _networkDelay;
    return Column(
      children: [
        const Text("Time Metrics", style: headerTextStyle),
        Wrap(
          children: [
            dataChip(
                "RPC Latency", "${state.baselineRpcLatency.inMilliseconds} ms"),
            dataChip(
                "Network Delay",
                (networkDelay != null)
                    ? "${networkDelay.inMilliseconds} ms"
                    : "N/A"),
            StreamBuilder(
                stream: Stream.periodic(const Duration(seconds: 1)),
                builder: (_, __) =>
                    dataChip("Global Slot", _globalSlot.toString())),
            dataChip("Epoch", _globalEpoch.toString()),
            dataChip("Epoch Completion",
                "${(_epochCompletion * 100).toStringAsFixed(1)} %"),
          ],
        )
      ],
    );
  }

  // Provides an estimate of "network/compute delay", meaning the time it took
  // for a block to propagate and validate across the network to the target node
  Duration? get _networkDelay {
    int totalDelta = 0;
    int count = 0;
    for (int i = 0; i < state.recentBlocks.length; i++) {
      final adoptionTime = state.recentAdoptionTimestamps[i];
      if (adoptionTime != null) {
        final adoptionTimeMs = adoptionTime.millisecondsSinceEpoch;
        final mintedTime =
            state.blocks[state.recentBlocks[i]]!.header.timestamp.toInt();
        final delta = (adoptionTimeMs - mintedTime);
        final withoutRpcDelay = delta - state.baselineRpcLatency.inMilliseconds;
        totalDelta += withoutRpcDelay;
        count++;
      }
    }
    if (count > 0) {
      return Duration(milliseconds: totalDelta ~/ count);
    } else {
      return null;
    }
  }

  Int64 get _globalSlot {
    final slotDurationProto = state.protocol.slotDuration;
    final slotDuration = Duration(
        seconds: slotDurationProto.seconds.toInt(),
        microseconds: slotDurationProto.nanos ~/ 1000);
    final genesisTimestamp = state.genesis.header.timestamp;
    final elapsedMs =
        Int64(DateTime.now().millisecondsSinceEpoch) - genesisTimestamp;
    final globalSlot = elapsedMs ~/ Int64(slotDuration.inMilliseconds);
    return globalSlot;
  }

  Int64 get _globalEpoch {
    final epochLength = state.nodeConfig.epochLength;
    final globalSlot = _globalSlot;
    final epoch = globalSlot ~/ epochLength;
    return epoch;
  }

  double get _epochCompletion {
    final epochLength = state.nodeConfig.epochLength;
    final globalSlot = _globalSlot;
    final subSlot = globalSlot % epochLength;
    return subSlot.toDouble() / epochLength.toDouble();
  }
}
