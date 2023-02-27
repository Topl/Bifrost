# DevContainer Testnet Simulator Guide
Instructions for running a testnet simulation in a Devcontainer environment (i.e. GitHub Codespaces).

## Installation
All pre-requisites should be automatically installed, including: Java, SBT, Docker, minikube, kubectl, helm, and k9s

### Bifrost
1. Run `sbt Docker/publishLocal` from the command line.
1. Run `./docs/scripts/docker_tag_minikube.sh` from the command line.

### Helm Charts
1. Run `cd /workspaces`
1. Run `git clone https://github.com/Topl/helm-charts.git`

## Run Simulation
1. From terminal, run (modify `my-testnet-name` accordingly)
   ```
   DEMO_NAME=my-testnet-name; \
    cd /workspaces &&
    helm upgrade --install \
    -f ./helm-charts/charts/bifrost-consensus-testnet/testnet-simple.yaml \
    --namespace $DEMO_NAME \
    --create-namespace $DEMO_NAME \
    ./helm-charts/charts/bifrost-consensus-testnet/ \
    && kubectl wait --timeout=-1s --for=condition=Ready pod/$DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME \
    && kubectl logs --follow $DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME
    ```
    Note: You can specify a different configuration file by changing the `-f` argument.
1. View the [results](https://console.cloud.google.com/storage/browser/bifrost-topl-labs-testnet-scenario-results/%2Fsimulation/results)
