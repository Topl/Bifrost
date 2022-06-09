///
//  Generated code. Do not modify.
//  source: protobuf/topl_grpc.proto
//
// @dart = 2.12
// ignore_for_file: annotate_overrides,camel_case_types,unnecessary_const,non_constant_identifier_names,library_prefixes,unused_import,unused_shown_name,return_of_invalid_type,unnecessary_this,prefer_final_fields

import 'dart:core' as $core;

import 'package:fixnum/fixnum.dart' as $fixnum;
import 'package:protobuf/protobuf.dart' as $pb;

class BroadcastTransactionReq extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BroadcastTransactionReq', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..a<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'transmittableBytes', $pb.PbFieldType.OY, protoName: 'transmittableBytes')
    ..hasRequiredFields = false
  ;

  BroadcastTransactionReq._() : super();
  factory BroadcastTransactionReq({
    $core.List<$core.int>? transmittableBytes,
  }) {
    final _result = create();
    if (transmittableBytes != null) {
      _result.transmittableBytes = transmittableBytes;
    }
    return _result;
  }
  factory BroadcastTransactionReq.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BroadcastTransactionReq.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BroadcastTransactionReq clone() => BroadcastTransactionReq()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BroadcastTransactionReq copyWith(void Function(BroadcastTransactionReq) updates) => super.copyWith((message) => updates(message as BroadcastTransactionReq)) as BroadcastTransactionReq; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BroadcastTransactionReq create() => BroadcastTransactionReq._();
  BroadcastTransactionReq createEmptyInstance() => create();
  static $pb.PbList<BroadcastTransactionReq> createRepeated() => $pb.PbList<BroadcastTransactionReq>();
  @$core.pragma('dart2js:noInline')
  static BroadcastTransactionReq getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BroadcastTransactionReq>(create);
  static BroadcastTransactionReq? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.int> get transmittableBytes => $_getN(0);
  @$pb.TagNumber(1)
  set transmittableBytes($core.List<$core.int> v) { $_setBytes(0, v); }
  @$pb.TagNumber(1)
  $core.bool hasTransmittableBytes() => $_has(0);
  @$pb.TagNumber(1)
  void clearTransmittableBytes() => clearField(1);
}

class BroadcastTransactionRes extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BroadcastTransactionRes', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..hasRequiredFields = false
  ;

  BroadcastTransactionRes._() : super();
  factory BroadcastTransactionRes() => create();
  factory BroadcastTransactionRes.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BroadcastTransactionRes.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BroadcastTransactionRes clone() => BroadcastTransactionRes()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BroadcastTransactionRes copyWith(void Function(BroadcastTransactionRes) updates) => super.copyWith((message) => updates(message as BroadcastTransactionRes)) as BroadcastTransactionRes; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BroadcastTransactionRes create() => BroadcastTransactionRes._();
  BroadcastTransactionRes createEmptyInstance() => create();
  static $pb.PbList<BroadcastTransactionRes> createRepeated() => $pb.PbList<BroadcastTransactionRes>();
  @$core.pragma('dart2js:noInline')
  static BroadcastTransactionRes getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BroadcastTransactionRes>(create);
  static BroadcastTransactionRes? _defaultInstance;
}

class BlockAdoptionsReq extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BlockAdoptionsReq', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..hasRequiredFields = false
  ;

  BlockAdoptionsReq._() : super();
  factory BlockAdoptionsReq() => create();
  factory BlockAdoptionsReq.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BlockAdoptionsReq.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BlockAdoptionsReq clone() => BlockAdoptionsReq()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BlockAdoptionsReq copyWith(void Function(BlockAdoptionsReq) updates) => super.copyWith((message) => updates(message as BlockAdoptionsReq)) as BlockAdoptionsReq; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BlockAdoptionsReq create() => BlockAdoptionsReq._();
  BlockAdoptionsReq createEmptyInstance() => create();
  static $pb.PbList<BlockAdoptionsReq> createRepeated() => $pb.PbList<BlockAdoptionsReq>();
  @$core.pragma('dart2js:noInline')
  static BlockAdoptionsReq getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BlockAdoptionsReq>(create);
  static BlockAdoptionsReq? _defaultInstance;
}

class BlockAdoptionsRes extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BlockAdoptionsRes', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..a<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'blockId', $pb.PbFieldType.OY, protoName: 'blockId')
    ..hasRequiredFields = false
  ;

  BlockAdoptionsRes._() : super();
  factory BlockAdoptionsRes({
    $core.List<$core.int>? blockId,
  }) {
    final _result = create();
    if (blockId != null) {
      _result.blockId = blockId;
    }
    return _result;
  }
  factory BlockAdoptionsRes.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BlockAdoptionsRes.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BlockAdoptionsRes clone() => BlockAdoptionsRes()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BlockAdoptionsRes copyWith(void Function(BlockAdoptionsRes) updates) => super.copyWith((message) => updates(message as BlockAdoptionsRes)) as BlockAdoptionsRes; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BlockAdoptionsRes create() => BlockAdoptionsRes._();
  BlockAdoptionsRes createEmptyInstance() => create();
  static $pb.PbList<BlockAdoptionsRes> createRepeated() => $pb.PbList<BlockAdoptionsRes>();
  @$core.pragma('dart2js:noInline')
  static BlockAdoptionsRes getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BlockAdoptionsRes>(create);
  static BlockAdoptionsRes? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.int> get blockId => $_getN(0);
  @$pb.TagNumber(1)
  set blockId($core.List<$core.int> v) { $_setBytes(0, v); }
  @$pb.TagNumber(1)
  $core.bool hasBlockId() => $_has(0);
  @$pb.TagNumber(1)
  void clearBlockId() => clearField(1);
}

class FetchBlockHeaderReq extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'FetchBlockHeaderReq', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..a<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'blockId', $pb.PbFieldType.OY, protoName: 'blockId')
    ..hasRequiredFields = false
  ;

  FetchBlockHeaderReq._() : super();
  factory FetchBlockHeaderReq({
    $core.List<$core.int>? blockId,
  }) {
    final _result = create();
    if (blockId != null) {
      _result.blockId = blockId;
    }
    return _result;
  }
  factory FetchBlockHeaderReq.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory FetchBlockHeaderReq.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  FetchBlockHeaderReq clone() => FetchBlockHeaderReq()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  FetchBlockHeaderReq copyWith(void Function(FetchBlockHeaderReq) updates) => super.copyWith((message) => updates(message as FetchBlockHeaderReq)) as FetchBlockHeaderReq; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static FetchBlockHeaderReq create() => FetchBlockHeaderReq._();
  FetchBlockHeaderReq createEmptyInstance() => create();
  static $pb.PbList<FetchBlockHeaderReq> createRepeated() => $pb.PbList<FetchBlockHeaderReq>();
  @$core.pragma('dart2js:noInline')
  static FetchBlockHeaderReq getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<FetchBlockHeaderReq>(create);
  static FetchBlockHeaderReq? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.int> get blockId => $_getN(0);
  @$pb.TagNumber(1)
  set blockId($core.List<$core.int> v) { $_setBytes(0, v); }
  @$pb.TagNumber(1)
  $core.bool hasBlockId() => $_has(0);
  @$pb.TagNumber(1)
  void clearBlockId() => clearField(1);
}

class FetchBlockHeaderRes extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'FetchBlockHeaderRes', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..aOM<BlockHeader>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'header', subBuilder: BlockHeader.create)
    ..hasRequiredFields = false
  ;

  FetchBlockHeaderRes._() : super();
  factory FetchBlockHeaderRes({
    BlockHeader? header,
  }) {
    final _result = create();
    if (header != null) {
      _result.header = header;
    }
    return _result;
  }
  factory FetchBlockHeaderRes.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory FetchBlockHeaderRes.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  FetchBlockHeaderRes clone() => FetchBlockHeaderRes()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  FetchBlockHeaderRes copyWith(void Function(FetchBlockHeaderRes) updates) => super.copyWith((message) => updates(message as FetchBlockHeaderRes)) as FetchBlockHeaderRes; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static FetchBlockHeaderRes create() => FetchBlockHeaderRes._();
  FetchBlockHeaderRes createEmptyInstance() => create();
  static $pb.PbList<FetchBlockHeaderRes> createRepeated() => $pb.PbList<FetchBlockHeaderRes>();
  @$core.pragma('dart2js:noInline')
  static FetchBlockHeaderRes getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<FetchBlockHeaderRes>(create);
  static FetchBlockHeaderRes? _defaultInstance;

  @$pb.TagNumber(1)
  BlockHeader get header => $_getN(0);
  @$pb.TagNumber(1)
  set header(BlockHeader v) { setField(1, v); }
  @$pb.TagNumber(1)
  $core.bool hasHeader() => $_has(0);
  @$pb.TagNumber(1)
  void clearHeader() => clearField(1);
  @$pb.TagNumber(1)
  BlockHeader ensureHeader() => $_ensure(0);
}

class BlockHeader extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BlockHeader', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..a<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'parentHeaderIdBytes', $pb.PbFieldType.OY, protoName: 'parentHeaderIdBytes')
    ..aInt64(2, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'parentSlot', protoName: 'parentSlot')
    ..a<$core.List<$core.int>>(3, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'txRootBytes', $pb.PbFieldType.OY, protoName: 'txRootBytes')
    ..a<$core.List<$core.int>>(4, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'bloomFilterBytes', $pb.PbFieldType.OY, protoName: 'bloomFilterBytes')
    ..aInt64(5, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'timestamp')
    ..aInt64(6, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'height')
    ..aInt64(7, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'slot')
    ..a<$core.List<$core.int>>(8, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'eligibilityCertificateBytes', $pb.PbFieldType.OY, protoName: 'eligibilityCertificateBytes')
    ..a<$core.List<$core.int>>(9, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'operationalCertificateBytes', $pb.PbFieldType.OY, protoName: 'operationalCertificateBytes')
    ..a<$core.List<$core.int>>(10, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'metadataBytes', $pb.PbFieldType.OY, protoName: 'metadataBytes')
    ..a<$core.List<$core.int>>(11, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'addressBytes', $pb.PbFieldType.OY, protoName: 'addressBytes')
    ..hasRequiredFields = false
  ;

  BlockHeader._() : super();
  factory BlockHeader({
    $core.List<$core.int>? parentHeaderIdBytes,
    $fixnum.Int64? parentSlot,
    $core.List<$core.int>? txRootBytes,
    $core.List<$core.int>? bloomFilterBytes,
    $fixnum.Int64? timestamp,
    $fixnum.Int64? height,
    $fixnum.Int64? slot,
    $core.List<$core.int>? eligibilityCertificateBytes,
    $core.List<$core.int>? operationalCertificateBytes,
    $core.List<$core.int>? metadataBytes,
    $core.List<$core.int>? addressBytes,
  }) {
    final _result = create();
    if (parentHeaderIdBytes != null) {
      _result.parentHeaderIdBytes = parentHeaderIdBytes;
    }
    if (parentSlot != null) {
      _result.parentSlot = parentSlot;
    }
    if (txRootBytes != null) {
      _result.txRootBytes = txRootBytes;
    }
    if (bloomFilterBytes != null) {
      _result.bloomFilterBytes = bloomFilterBytes;
    }
    if (timestamp != null) {
      _result.timestamp = timestamp;
    }
    if (height != null) {
      _result.height = height;
    }
    if (slot != null) {
      _result.slot = slot;
    }
    if (eligibilityCertificateBytes != null) {
      _result.eligibilityCertificateBytes = eligibilityCertificateBytes;
    }
    if (operationalCertificateBytes != null) {
      _result.operationalCertificateBytes = operationalCertificateBytes;
    }
    if (metadataBytes != null) {
      _result.metadataBytes = metadataBytes;
    }
    if (addressBytes != null) {
      _result.addressBytes = addressBytes;
    }
    return _result;
  }
  factory BlockHeader.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BlockHeader.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BlockHeader clone() => BlockHeader()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BlockHeader copyWith(void Function(BlockHeader) updates) => super.copyWith((message) => updates(message as BlockHeader)) as BlockHeader; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BlockHeader create() => BlockHeader._();
  BlockHeader createEmptyInstance() => create();
  static $pb.PbList<BlockHeader> createRepeated() => $pb.PbList<BlockHeader>();
  @$core.pragma('dart2js:noInline')
  static BlockHeader getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BlockHeader>(create);
  static BlockHeader? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.int> get parentHeaderIdBytes => $_getN(0);
  @$pb.TagNumber(1)
  set parentHeaderIdBytes($core.List<$core.int> v) { $_setBytes(0, v); }
  @$pb.TagNumber(1)
  $core.bool hasParentHeaderIdBytes() => $_has(0);
  @$pb.TagNumber(1)
  void clearParentHeaderIdBytes() => clearField(1);

  @$pb.TagNumber(2)
  $fixnum.Int64 get parentSlot => $_getI64(1);
  @$pb.TagNumber(2)
  set parentSlot($fixnum.Int64 v) { $_setInt64(1, v); }
  @$pb.TagNumber(2)
  $core.bool hasParentSlot() => $_has(1);
  @$pb.TagNumber(2)
  void clearParentSlot() => clearField(2);

  @$pb.TagNumber(3)
  $core.List<$core.int> get txRootBytes => $_getN(2);
  @$pb.TagNumber(3)
  set txRootBytes($core.List<$core.int> v) { $_setBytes(2, v); }
  @$pb.TagNumber(3)
  $core.bool hasTxRootBytes() => $_has(2);
  @$pb.TagNumber(3)
  void clearTxRootBytes() => clearField(3);

  @$pb.TagNumber(4)
  $core.List<$core.int> get bloomFilterBytes => $_getN(3);
  @$pb.TagNumber(4)
  set bloomFilterBytes($core.List<$core.int> v) { $_setBytes(3, v); }
  @$pb.TagNumber(4)
  $core.bool hasBloomFilterBytes() => $_has(3);
  @$pb.TagNumber(4)
  void clearBloomFilterBytes() => clearField(4);

  @$pb.TagNumber(5)
  $fixnum.Int64 get timestamp => $_getI64(4);
  @$pb.TagNumber(5)
  set timestamp($fixnum.Int64 v) { $_setInt64(4, v); }
  @$pb.TagNumber(5)
  $core.bool hasTimestamp() => $_has(4);
  @$pb.TagNumber(5)
  void clearTimestamp() => clearField(5);

  @$pb.TagNumber(6)
  $fixnum.Int64 get height => $_getI64(5);
  @$pb.TagNumber(6)
  set height($fixnum.Int64 v) { $_setInt64(5, v); }
  @$pb.TagNumber(6)
  $core.bool hasHeight() => $_has(5);
  @$pb.TagNumber(6)
  void clearHeight() => clearField(6);

  @$pb.TagNumber(7)
  $fixnum.Int64 get slot => $_getI64(6);
  @$pb.TagNumber(7)
  set slot($fixnum.Int64 v) { $_setInt64(6, v); }
  @$pb.TagNumber(7)
  $core.bool hasSlot() => $_has(6);
  @$pb.TagNumber(7)
  void clearSlot() => clearField(7);

  @$pb.TagNumber(8)
  $core.List<$core.int> get eligibilityCertificateBytes => $_getN(7);
  @$pb.TagNumber(8)
  set eligibilityCertificateBytes($core.List<$core.int> v) { $_setBytes(7, v); }
  @$pb.TagNumber(8)
  $core.bool hasEligibilityCertificateBytes() => $_has(7);
  @$pb.TagNumber(8)
  void clearEligibilityCertificateBytes() => clearField(8);

  @$pb.TagNumber(9)
  $core.List<$core.int> get operationalCertificateBytes => $_getN(8);
  @$pb.TagNumber(9)
  set operationalCertificateBytes($core.List<$core.int> v) { $_setBytes(8, v); }
  @$pb.TagNumber(9)
  $core.bool hasOperationalCertificateBytes() => $_has(8);
  @$pb.TagNumber(9)
  void clearOperationalCertificateBytes() => clearField(9);

  @$pb.TagNumber(10)
  $core.List<$core.int> get metadataBytes => $_getN(9);
  @$pb.TagNumber(10)
  set metadataBytes($core.List<$core.int> v) { $_setBytes(9, v); }
  @$pb.TagNumber(10)
  $core.bool hasMetadataBytes() => $_has(9);
  @$pb.TagNumber(10)
  void clearMetadataBytes() => clearField(10);

  @$pb.TagNumber(11)
  $core.List<$core.int> get addressBytes => $_getN(10);
  @$pb.TagNumber(11)
  set addressBytes($core.List<$core.int> v) { $_setBytes(10, v); }
  @$pb.TagNumber(11)
  $core.bool hasAddressBytes() => $_has(10);
  @$pb.TagNumber(11)
  void clearAddressBytes() => clearField(11);
}

class CurrentMempoolReq extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'CurrentMempoolReq', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..hasRequiredFields = false
  ;

  CurrentMempoolReq._() : super();
  factory CurrentMempoolReq() => create();
  factory CurrentMempoolReq.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory CurrentMempoolReq.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  CurrentMempoolReq clone() => CurrentMempoolReq()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  CurrentMempoolReq copyWith(void Function(CurrentMempoolReq) updates) => super.copyWith((message) => updates(message as CurrentMempoolReq)) as CurrentMempoolReq; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static CurrentMempoolReq create() => CurrentMempoolReq._();
  CurrentMempoolReq createEmptyInstance() => create();
  static $pb.PbList<CurrentMempoolReq> createRepeated() => $pb.PbList<CurrentMempoolReq>();
  @$core.pragma('dart2js:noInline')
  static CurrentMempoolReq getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<CurrentMempoolReq>(create);
  static CurrentMempoolReq? _defaultInstance;
}

class CurrentMempoolRes extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'CurrentMempoolRes', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..p<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'transactionIds', $pb.PbFieldType.PY, protoName: 'transactionIds')
    ..hasRequiredFields = false
  ;

  CurrentMempoolRes._() : super();
  factory CurrentMempoolRes({
    $core.Iterable<$core.List<$core.int>>? transactionIds,
  }) {
    final _result = create();
    if (transactionIds != null) {
      _result.transactionIds.addAll(transactionIds);
    }
    return _result;
  }
  factory CurrentMempoolRes.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory CurrentMempoolRes.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  CurrentMempoolRes clone() => CurrentMempoolRes()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  CurrentMempoolRes copyWith(void Function(CurrentMempoolRes) updates) => super.copyWith((message) => updates(message as CurrentMempoolRes)) as CurrentMempoolRes; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static CurrentMempoolRes create() => CurrentMempoolRes._();
  CurrentMempoolRes createEmptyInstance() => create();
  static $pb.PbList<CurrentMempoolRes> createRepeated() => $pb.PbList<CurrentMempoolRes>();
  @$core.pragma('dart2js:noInline')
  static CurrentMempoolRes getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<CurrentMempoolRes>(create);
  static CurrentMempoolRes? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.List<$core.int>> get transactionIds => $_getList(0);
}

