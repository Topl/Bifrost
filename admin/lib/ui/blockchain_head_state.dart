import 'package:admin/state.dart';
import 'package:admin/ui/utils.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:flutter/material.dart';

class BlockchainHeadState extends StatelessWidget {
  final BlockchainState state;

  const BlockchainHeadState({super.key, required this.state});

  @override
  Widget build(BuildContext context) {
    final head = state.blocks[state.currentHeadId]!;
    return Column(
      children: [
        const Text("Canonical Head", style: headerTextStyle),
        Wrap(
          children: [
            dataChip("Block ID", Base58Encode(state.currentHeadId.value)),
            const VerticalDivider(),
            dataChip("Height", head.header.height.toString()),
            const VerticalDivider(),
            dataChip("Slot", head.header.slot.toString()),
          ],
        ),
      ],
    );
  }
}
