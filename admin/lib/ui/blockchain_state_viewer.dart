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
        _paddedCard(
            child: BlockchainHeadState(state: state),
            color: hash32ToLightColor(state.currentHeadId.value)),
        _paddedCard(child: BlockchainTimeInfo(state: state)),
        _paddedCard(child: BlockProductionRate(state: state)),
        _paddedCard(child: StakerDistribution(state: state)),
        _paddedCard(child: BlockTimeCharts(state: state)),
      ],
    );
  }

  Widget _paddedCard({required Widget child, Color? color}) => Card(
      color: color,
      child: Padding(
        padding: const EdgeInsets.all(8.0),
        child: child,
      ));
}
