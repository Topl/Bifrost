///
//  Generated code. Do not modify.
//  source: protobuf/topl_grpc.proto
//
// @dart = 2.12
// ignore_for_file: annotate_overrides,camel_case_types,unnecessary_const,non_constant_identifier_names,library_prefixes,unused_import,unused_shown_name,return_of_invalid_type,unnecessary_this,prefer_final_fields

import 'dart:core' as $core;

import 'package:protobuf/protobuf.dart' as $pb;

class BroadcastTxReq extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BroadcastTxReq', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..a<$core.List<$core.int>>(1, const $core.bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'transmittableBytes', $pb.PbFieldType.OY, protoName: 'transmittableBytes')
    ..hasRequiredFields = false
  ;

  BroadcastTxReq._() : super();
  factory BroadcastTxReq({
    $core.List<$core.int>? transmittableBytes,
  }) {
    final _result = create();
    if (transmittableBytes != null) {
      _result.transmittableBytes = transmittableBytes;
    }
    return _result;
  }
  factory BroadcastTxReq.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BroadcastTxReq.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BroadcastTxReq clone() => BroadcastTxReq()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BroadcastTxReq copyWith(void Function(BroadcastTxReq) updates) => super.copyWith((message) => updates(message as BroadcastTxReq)) as BroadcastTxReq; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BroadcastTxReq create() => BroadcastTxReq._();
  BroadcastTxReq createEmptyInstance() => create();
  static $pb.PbList<BroadcastTxReq> createRepeated() => $pb.PbList<BroadcastTxReq>();
  @$core.pragma('dart2js:noInline')
  static BroadcastTxReq getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BroadcastTxReq>(create);
  static BroadcastTxReq? _defaultInstance;

  @$pb.TagNumber(1)
  $core.List<$core.int> get transmittableBytes => $_getN(0);
  @$pb.TagNumber(1)
  set transmittableBytes($core.List<$core.int> v) { $_setBytes(0, v); }
  @$pb.TagNumber(1)
  $core.bool hasTransmittableBytes() => $_has(0);
  @$pb.TagNumber(1)
  void clearTransmittableBytes() => clearField(1);
}

class BroadcastTxRes extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = $pb.BuilderInfo(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'BroadcastTxRes', package: const $pb.PackageName(const $core.bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'co.topl.grpc.services'), createEmptyInstance: create)
    ..hasRequiredFields = false
  ;

  BroadcastTxRes._() : super();
  factory BroadcastTxRes() => create();
  factory BroadcastTxRes.fromBuffer($core.List<$core.int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromBuffer(i, r);
  factory BroadcastTxRes.fromJson($core.String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) => create()..mergeFromJson(i, r);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
  'Will be removed in next major version')
  BroadcastTxRes clone() => BroadcastTxRes()..mergeFromMessage(this);
  @$core.Deprecated(
  'Using this can add significant overhead to your binary. '
  'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
  'Will be removed in next major version')
  BroadcastTxRes copyWith(void Function(BroadcastTxRes) updates) => super.copyWith((message) => updates(message as BroadcastTxRes)) as BroadcastTxRes; // ignore: deprecated_member_use
  $pb.BuilderInfo get info_ => _i;
  @$core.pragma('dart2js:noInline')
  static BroadcastTxRes create() => BroadcastTxRes._();
  BroadcastTxRes createEmptyInstance() => create();
  static $pb.PbList<BroadcastTxRes> createRepeated() => $pb.PbList<BroadcastTxRes>();
  @$core.pragma('dart2js:noInline')
  static BroadcastTxRes getDefault() => _defaultInstance ??= $pb.GeneratedMessage.$_defaultFor<BroadcastTxRes>(create);
  static BroadcastTxRes? _defaultInstance;
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

