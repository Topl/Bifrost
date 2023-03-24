class P2PException implements Exception {
  final String reason;

  P2PException(this.reason);

  String toString() => "P2PException: $reason";
}
