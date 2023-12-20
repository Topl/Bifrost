import 'package:admin/state.dart';
import 'package:admin/ui/utils.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';
import 'package:topl_common/proto/consensus/models/staking.pb.dart';

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
        SizedBox(
          width: 400,
          height: 400,
          child: Row(
            children: [
              const SizedBox(height: 18),
              Expanded(child: pieChart),
              legend,
              const SizedBox(height: 18),
            ],
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
