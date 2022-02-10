# Genus Event Sourcing Server

## Configuration

The current configuration settings are hard-coded in the `GenusApp` Scala object located at
`genus/src/main/scala/co/topl/genus/GenusApp.scala`.

The current hard-coded options are:
- `serverIp`
- `serverPort`
- `mongoConnectionString`
- `databaseName`
- `txsCollectionName`
- `blocksCollectionName`

## Running The Service

The Genus service can be started using the command `sbt "genus / run"` from the root of the Bifrost repository.

## Generating Client Code

See [the official GRPC site](https://grpc.io/docs/languages/) for information on generating GRPC clients for your
language of choice. Any official clients supported by Topl will be listed here.

The GRPC services and types protobuf files are located in `genus/src/main/protobuf`.

## GRPC-Web Support

Genus can be accessed via GRPC-Web by running an Envoy proxy server pointing to a running Genus instance.
The Envoy proxy will convert incoming GRPC-web compliant requests intto GRPC calls which are then sent to Genus.
Outgoing responses from Genus are converted back into GRPC-Web HTTP-compliant responses.

To run Envoy, first install the [Envoy executable](https://www.envoyproxy.io/docs/envoy/latest/start/install), then
run `envoy -c ./genus/src/main/resources/envoy-grpc-web.yaml` from the root Bifrost directory. Additional configuration
settings can be found in the [Envoy Configuration Reference](https://www.envoyproxy.io/docs/envoy/latest/configuration/configuration).

### GRPC Web Clients

- [NPM grpc-web](https://www.npmjs.com/package/grpc-web)
- [NPM grpc-web-rx](https://www.npmjs.com/package/grpc-web-rx)
- [Dart grpc](https://pub.dev/packages/grpc)
  - [example code](https://github.com/grpc/grpc-dart/tree/master/example/grpc-web)
