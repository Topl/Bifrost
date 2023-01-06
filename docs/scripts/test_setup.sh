#!/usr/bin/env bash

sudo apt update

sudo apt install snapd

sudo mkdir -p ~/.kube

sudo chown -f -R $USER ~/.kube

echo "alias kubectl='microk8s kubectl'" >> ~/.bashrc

sudo snap install microk8s --classic --channel=1.26

sudo usermod -a -G microk8s $USER

# we use runuser here to get around having to re-load all the group information,
# 	which requires user interaction

sudo runuser -l $USER -c 'microk8s status --wait-ready'

sudo runuser -l $USER -c 'microk8s enable dns helm helm3 hostpath-storage registry storage'

sudo runuser -l $USER -c 'cd ~/Bifrost/ && source "$HOME/.sdkman/bin/sdkman-init.sh" && sbt "Docker / publishLocal"'

sudo runuser -l $USER -c 'docker tag toplprotocol/network-delayer localhost:32000/topl/network-delayer && docker push localhost:32000/topl/network-delayer'

sudo runuser -l $USER -c 'docker tag toplprotocol/bifrost-node localhost:32000/topl/bifrost-node && docker push localhost:32000/topl/bifrost-node'

sudo runuser -l $USER -c 'docker tag toplprotocol/testnet-simulation-orchestrator localhost:32000/topl/testnet-simulation-orchestrator && docker push localhost:32000/topl/testnet-simulation-orchestrator'
