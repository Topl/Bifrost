# Bifrost Development Setup
Bifrost officially supports development on Ubuntu.  Other operating systems may still work, but the Topl team may be unable to support certain questions.

## Setup via Script
A [convenience script](./scripts/dev_setup.sh) is provided which will run the setup automatically. Recommended practice is to read the contents of the script before running it. From the command line, within the the repository root:

```sh
cat ./docs/scripts/dev_setup.sh
source ./docs/scripts/dev_setup.sh
```

## Setup Manually
1. Install [Docker](https://docs.docker.com/engine/install/).
    1. Be sure to follow the Linux post-installation [steps](https://docs.docker.com/engine/install/linux-postinstall/), namely run `sudo usermod -aG docker $USER`
1. Install Java and SBT using [SDKMAN](https://sdkman.io/install).
    1. Prerequisite: Install zip and unzip `sudo apt install zip unzip`
    1. Run `curl -s "https://get.sdkman.io" | bash`
    1. Run `source "$HOME/.sdkman/bin/sdkman-init.sh"`
    1. Run `sdk install java 11.0.17-tem`
    1. Run `sdk install sbt 1.7.3`

## Build & Test
```sh
git clone https://github.com/Topl/Bifrost
cd Bifrost
git checkout tetra
sbt compile
sbt test
```

### Docker Images

To publish a Docker image from the project directory for local testing: `sbt 'node/docker:publishLocal'`
   
   - To run the published container: `docker run bifrost:x.x.x` (where `x.x.x` is the version that was published).
   - To pass command line arguments: `docker run bifrost:x.x.x -s mySpecialSeed`