import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_crypto/kes.dart';
import 'package:bifrost_node/genesis.dart';
import 'package:bifrost_node/staker_initializer.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fixnum/fixnum.dart';
import 'package:rational/rational.dart';
import 'package:topl_protobuf/brambl/models/address.pb.dart';
import 'package:topl_protobuf/brambl/models/box/value.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/unspent_transaction_output.pb.dart';

class PrivateTestnet {
  static final DefaultTotalStake = BigInt.from(10000000);

  Future<List<StakerInitializer>> stakerInitializers(
      Int64 timestamp, int stakerCount) async {
    assert(stakerCount >= 0);
    final out = <StakerInitializer>[];
    for (int i = 0; i < stakerCount; i++) {
      final seed = (await Sha256().hash(<int>[]
            ..addAll(timestamp.immutableBytes)
            ..addAll(i.immutableBytes)))
          .bytes;
      out.add(await StakerInitializer.fromSeed(seed, TreeHeight(9, 9)));
    }
    return out;
  }

  Future<GenesisConfig> config(Int64 timestamp, List<StakerInitializer> stakers,
      List<BigInt>? stakes) async {
    final someStakes = stakes ??
        List.filled(stakers.length,
            Rational(DefaultTotalStake, BigInt.from(stakers.length)).round());
    final outputs = [
      UnspentTransactionOutput(
          address: HeightLockOneSpendingAddress,
          value:
              Value(lvl: Value_LVL(quantity: BigInt.from(10000000).toInt128))),
    ];
    for (int i = 0; i < stakers.length; i++) {
      final staker = stakers[i];
      final stake = someStakes[i];
      final genesisOutputs = await staker.genesisOutputs(stake.toInt128);
      outputs.addAll(genesisOutputs);
    }

    return GenesisConfig(timestamp, outputs, GenesisConfig.DefaultEtaPrefix);
  }
}

// TODO
final HeightLockOneSpendingAddress = LockAddress();
