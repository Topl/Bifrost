import 'package:bifrost_crypto/ed25519.dart';
import 'package:bifrost_crypto/ed25519vrf.dart';
import 'package:bifrost_crypto/kes.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/brambl/models/address.pb.dart';
import 'package:topl_protobuf/brambl/models/box/value.pb.dart';
import 'package:topl_protobuf/brambl/models/evidence.pb.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/unspent_transaction_output.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/quivr/models/shared.pb.dart';

class StakerInitializer {
  final Ed25519KeyPair operatorKeyPair;
  final Ed25519KeyPair walletKeyPair;
  final Ed25519KeyPair spendingKeyPair;
  final Ed25519VRFKeyPair vrfKeyPair;
  final KeyPairKesProduct kesKeyPair;

  StakerInitializer(this.operatorKeyPair, this.walletKeyPair,
      this.spendingKeyPair, this.vrfKeyPair, this.kesKeyPair);

  static Future<StakerInitializer> fromSeed(
      List<int> seed, TreeHeight treeHeight) async {
    final operatorKeyPair = await ed25519.generateKeyPairFromSeed(seed);
    final walletKeyPair = await ed25519.generateKeyPairFromSeed(seed);
    final spendingKeyPair = await ed25519.generateKeyPairFromSeed(seed);
    final vrfKeyPair = await ed25519Vrf.generateKeyPairFromSeed(seed);
    final kesKeyPair =
        await kesProduct.generateKeyPair(seed, treeHeight, Int64.ZERO);

    return StakerInitializer(
      operatorKeyPair,
      walletKeyPair,
      spendingKeyPair,
      vrfKeyPair,
      kesKeyPair,
    );
  }

  Future<SignatureKesProduct> get registration async => kesProduct.sign(
      kesKeyPair.sk,
      (await Sha256().hash(<int>[]
            ..addAll(vrfKeyPair.vk)
            ..addAll(operatorKeyPair.vk)))
          .bytes);

  StakingAddress get stakingAddress =>
      StakingAddress(value: operatorKeyPair.vk);

  // TODO
  LockAddress get lockAddress => LockAddress(
      lock32: Identifier_Lock32(
          evidence: Evidence_Sized32(
              digest: Digest_Digest32(value: List.filled(32, 0x00)))));

  Future<List<UnspentTransactionOutput>> genesisOutputs(Int128 stake) async {
    final toplValue = Value(
        topl: Value_TOPL(quantity: stake, stakingAddress: stakingAddress));
    final registrationValue = Value(
        registration: Value_Registration(
            registration: await registration, stakingAddress: stakingAddress));
    return [
      UnspentTransactionOutput(address: lockAddress, value: toplValue),
      UnspentTransactionOutput(address: lockAddress, value: registrationValue)
    ];
  }
}
