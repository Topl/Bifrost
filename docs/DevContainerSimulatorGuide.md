# DevContainer Testnet Simulator Guide
Instructions for running a testnet simulation in a Devcontainer environment (i.e. GitHub Codespaces).

## Devcontainer
Devcontainers are a way to package your dev enviornment and run it from a Docker container (or remotely via GitHub Codespaces).

The configuration is stored in the `.devcontainer` folder which contains a `devcontainer.json` file as well as a few install scripts. 

Our devcontainer setup installs:
* Docker (in Docker)
* Minikube
* SDKMan

### Use Devcontainer locally
To use devcontainers locally, you will need:

* Docker
* VSCode
* Devcontainer plugin for VSCode

Once these are installed, you can open the folder in VSCode, and it will prompt you to open the devcontainer environment. From there, you will be able to run the commands below.

## Installation
All pre-requisites should be automatically installed, including: Java, SBT, Docker, minikube, kubectl, helm, and k9s

### Bifrost
1. Run `sbt Docker/publishLocal` from the command line.
1. Run `./docs/scripts/docker_tag_minikube.sh` from the command line.

### Helm Charts
1. Run `cd /workspaces`
1. Run `sudo git clone https://github.com/Topl/helm-charts.git`

## Run Simulation
1. From terminal, run (modify `my-testnet-name` accordingly)
   ```
   DEMO_NAME=my-testnet-name; \
    cd /workspaces &&
    helm upgrade --install \
    -f ./helm-charts/examples/bifrost-consensus-testnet/testnet-simple.yaml \
    --namespace $DEMO_NAME \
    --create-namespace $DEMO_NAME \
    ./helm-charts/charts/bifrost-consensus-testnet/ \
    && kubectl wait --timeout=-1s --for=condition=Ready pod/$DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME \
    && kubectl logs --follow $DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME
    ```
    Note: You can specify a different configuration file by changing the `-f` argument.
1. View the [results](https://console.cloud.google.com/storage/browser/bifrost-topl-labs-testnet-scenario-results/%2Fsimulation/results)

### Troubleshooting

> StatefulSet.apps "DEMO_NAME" is invalid: metadata.name: Invalid value: "DEMO_NAME": must be no more than 63 characters

To fix, make sure that `DEMO_NAME` is less that 63 characters.
