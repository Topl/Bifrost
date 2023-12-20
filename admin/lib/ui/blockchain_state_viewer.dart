import 'package:admin/state.dart';
import 'package:admin/ui/block_production_rate.dart';
import 'package:admin/ui/block_time_charts.dart';
import 'package:admin/ui/blockchain_head_state.dart';
import 'package:admin/ui/blockchain_time_info.dart';
import 'package:admin/ui/staker_distribution.dart';
import 'package:admin/ui/utils.dart';
import 'package:flutter/material.dart';

class BlockchainStateViewer extends StatelessWidget {
  final BlockchainState state;

  const BlockchainStateViewer({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Card(
          color: hash32ToLightColor(state.currentHeadId.value),
          child: BlockchainHeadState(state: state),
        ),
        Card(child: BlockchainTimeInfo(state: state)),
        Card(child: BlockProductionRate(state: state)),
        Card(child: StakerDistribution(state: state)),
        Card(child: BlockTimeCharts(state: state)),
      ],
    );
  }
}
