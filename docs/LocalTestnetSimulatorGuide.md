# Local Testnet Simulator Guide
Instructions for running a testnet simulation in a local environment.

## Installation

### Development Setup
1. See [Development Setup](./DevelopmentSetup.md).

### Kubernetes
1. Install [Microk8s](https://microk8s.io/docs/install-alternatives).
1. Enable addons with command `microk8s enable dns helm helm3 hostpath-storage registry storage`

### Bifrost
1. Run `sbt "Docker / publishLocal"` from the command line.
1. Run `docker tag toplprotocol/network-delayer localhost:32000/topl/network-delayer && docker push localhost:32000/topl/network-delayer`
1. Run `docker tag toplprotocol/bifrost-node-tetra localhost:32000/topl/bifrost-node-tetra && docker push localhost:32000/topl/bifrost-node-tetra`
1. Run `docker tag toplprotocol/testnet-simulation-orchestrator localhost:32000/topl/testnet-simulation-orchestrator && docker push localhost:32000/topl/testnet-simulation-orchestrator`.

## Run Simulation
1. Open terminal.  `cd {bifrost repository}/helm`
1. Copy `bifrost-consensus-testnet/override-example.yaml` to a path on your machine.  Update values accordingly.
1. From terminal, run `microk8s helm install -f {path to override.yaml} --create-namespace --namespace {scenario name} {scenario name} ./bifrost-consensus-testnet`
1. View the [results](https://console.cloud.google.com/storage/browser/bifrost-topl-labs-testnet-scenario-results/%2Fsimulation/results)
