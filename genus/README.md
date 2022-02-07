# Genus Event Sourcing Server

## Configuration

The current configuration settings are hard-coded in the `GenusApp` Scala object located at
`genus/src/main/scala/co/topl/genus/GenusApp.scala`.

The current hard-coded options are:
- `serverIp`
- `serverPort`
- `mongoConnectionString`
- `mongoDatabaseName`
- `txsMongoCollectionName`
- `blocksMongoCollectionName`

## Running The Service

The Genus service can be started using the command `sbt "genus / run"` from the root of the Bifrost repository.

## Generating Client Code

See [the official GRPC site](https://grpc.io/docs/languages/) for info on generating GRPC clients for your language of
choice. Any official clients supported by Topl will be listed here.

The GRPC services and types protobuf files are located in `genus/src/main/protobuf`.

## GRPC-Web Support

Genus can be accessed via GRPC-Web by running an Envoy proxy server pointing to a running Genus instance.
The Envoy proxy will convert incoming GRPC-web compliant requests to GRPC calls which are sent to Genus.
Outgoing responses from Genus are then converted back into HTTP/1.1 GRPC-Web compliant responses.

To run Envoy, first install the [Envoy executable](https://www.envoyproxy.io/docs/envoy/latest/start/install), then
run `envoy -c ./genus/src/main/resources/envoy-grpc-web.yaml` from the root Bifrost directory. Additional configuration
settings can be found in the [Envoy Configuration Reference](https://www.envoyproxy.io/docs/envoy/latest/configuration/configuration).
