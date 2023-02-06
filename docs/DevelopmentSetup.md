# Bifrost Development Setup
Bifrost officially supports development on Ubuntu.  Other operating systems may still work, but the Topl team may be unable to support certain questions.

## Installation
A [convenience script](./scripts/dev_setup.sh) is provided which will run the following steps automatically.  We highly recommend reading the contents of the script before running it.  The convenience script can be run by typing `./docs/scripts/dev_steup.sh` from the command line (from the repository root).
Otherwise, the directions below will suffice.

### Docker, Java, and SBT
1. Install [Docker](https://docs.docker.com/engine/install/).
    1. Be sure to follow the Linux post-installation [steps](https://docs.docker.com/engine/install/linux-postinstall/), namely run `sudo usermod -aG docker $USER`
1. Install Java and SBT using [SDKMAN](https://sdkman.io/install).
    1. Prerequisite: Install zip and unzip `sudo apt install zip unzip`
    1. Run `curl -s "https://get.sdkman.io" | bash`
    1. Run `source "$HOME/.sdkman/bin/sdkman-init.sh"`
    1. Run `sdk install java 11.0.17-tem`
    1. Run `sdk install sbt 1.7.3`

## Bifrost
1. Clone https://github.com/Topl/Bifrost and checkout branch `tetra`.
1. Run `sbt compile` from the command line.
