import 'dart:typed_data';

import 'package:fixnum/fixnum.dart';
import 'package:flutter/material.dart';
import 'package:topl_common/proto/consensus/models/staking.pb.dart';

Widget dataChip(String header, String value) => Padding(
      padding: const EdgeInsets.all(8.0),
      child: Column(
        children: [
          Text(header, style: miniHeaderTextStyle),
          Chip(label: Text(value))
        ],
      ),
    );

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
