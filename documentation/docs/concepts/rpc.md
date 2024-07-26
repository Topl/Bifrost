# RPC
Users interact with the blockchain using the Remote Procedure Call (RPC) layer.  This layer is implemented using gRPC using the specification defined in [Topl/protobuf-specs](https://github.com/Topl/protobuf-specs).

In general, the RPC layer is bound to host `0.0.0.0` at port `9084`. Only gRPC over HTTP/2 is supported directly by the node. To use gRPC-web, a separate proxy like Envoy should be used.

The Node's gRPC layer is implemented using the interface provided by [Blockchain Core](blockchain-core).

Genus's gRPC layer is served using the same port. When running the (legacy) Node-embedded Genus, node traffic flows through the node interpreter while Genus traffic flows through Genus; but it's all served from the same JVM. When running Genus as a standalone server, node traffic is proxied to a remote node instance while Genus traffic is handled by the Genus server.
