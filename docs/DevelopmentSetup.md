# Bifrost Development Setup
Bifrost officially supports development on Ubuntu.  Other operating systems may still work, but the Topl team may be unable to support certain questions.

## Setup via Script
A [convenience script](./scripts/dev_setup.sh) is provided which will run the setup automatically. Recommended practice is to read the contents of the script before running it. From the command line, within the the repository root:

```sh
cat ./docs/scripts/dev_setup.sh
source ./docs/scripts/dev_setup.sh
```

## Setup Manually

You can use the contents of the `dev_setup.sh` script as a reference for a manual installation via the command line:

https://github.com/Topl/Bifrost/blob/440eae84a3b116ff86eeb833b1733711cf51fb70/docs/scripts/dev_setup.sh#L1-L31    

Alternatively, follow the original install instructions:

* Install [Docker](https://docs.docker.com/engine/install/)
* Be sure to follow the Linux post-installation [steps](https://docs.docker.com/engine/install/linux-postinstall/), namely run 
  * `sudo usermod -aG docker $USER`
* Install Java and SBT using [SDKMAN](https://sdkman.io/install)

## Fork, Clone, Build & Test

Fork the repository at https://github.com/Topl/Bifrost/fork

```sh
# replace <your-github-username> with your github account username
git clone https://github.com/<your-github-username>/Bifrost
cd Bifrost
git remote add upstream https://github.com/Topl/Bifrost.git

git checkout tetra
sbt compile
sbt test
```

## Docker Images

To publish a Docker image from the project directory for local testing: `sbt 'node/docker:publishLocal'`
   
   - To run the published container: `docker run bifrost:x.x.x` (where `x.x.x` is the version that was published).
   - To pass command line arguments: `docker run bifrost:x.x.x -s mySpecialSeed`