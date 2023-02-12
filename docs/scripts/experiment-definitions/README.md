# Experiment Generation Compiler

This directory contains the source code for the experiment generation compiler. The compiler takes in a higher-level
description of an experiment, including parametric sweeps and randomized topologies, and generates a set of
experiments that can be run by the local testnet simulator.

This tool is written for Python 3.11. We recommend using a virtual environment to install the dependencies, which are
listed in `requirements.txt`.

### Initial Setup

You should first ensure that you have Python 3.11 available on your system:

    $ python3.11 --version
    Python 3.11.0

If you do not have Python 3.11 installed, you can install it using your system's package manager. For example, on
Ubuntu:

    $ sudo add-apt-repository ppa:deadsnakes/ppa
    $ sudo apt update
    $ sudo apt install python3.11 python3.11-venv

Once you have Python 3.11 installed, you can create a virtual environment and install the dependencies:

    $ python3.11 -m venv venv
    $ source venv/bin/activate
    $ pip install -r requirements.txt

Now you are ready to run the compiler.

### Defining an Experiment Definition File

##### Template Configuration File

When designing an experiment, you will need to start by identifying a template configuration file to use as a
foundation.
This file will provide all the settings that you do NOT want to vary across experiments. For example, you might want
all results to go to the same output directory, or you might want all experiments to use the same network topology.

This template configuration file should be a YAML file that conforms to the experiment definition format. You must
include the path to this file in your experiment definition file using the `_template` key. For example:

    _template: /absolute/path/to/template.yaml
    <or>
    _template: relative/path/to/template.yaml

##### Overriding Settings

If you wish to override any settings in the template configuration file, you can do so by specifying them in your
experiment definition file. For example, if you want to override the `scenario/transactionsPerSecond` setting to
evaluate performance under different TPS regimes, you can do so by adding the following to your experiment definition
file:

    "scenario/transactionsPerSecond": [ 2.5, 5.0, 7.5 ]
    <or>
    "scenario/transactionsPerSecond": {"min": 2.5, "max": 7.5, "step": 0.5}

Here, the first entry specifies a list of values to test for the generated experiments (at least three),
while the second entry specifies a range of 11 values to use. In either case, the compiler will
generate new experiment configurations, each with a different value for the `scenario/transactionsPerSecond` setting.

##### Setting Topologies

You can also specify the network topology to use for each experiment. For example, if you want to evaluate the
performance of five random networks, each with 10 producers in an Erdos-Renyi topology, you can do so by
adding the following to your experiment definition file:

    _num_producers: [ 10 ]
    _topology_type: [ "erdos renyi" ]
    scenario/randomSeed: [ 0, 1, 2, 3, 4 ]

Note that the random of seeds is required to generate different topologies. If you do not specify a random seed,
the compiler will generate a single Erdos-Renyi topology with 10 producers using a seed of 0.

There are four keys used to define the topology of an experiment:

* `_num_producers`: The number of producers in the network. This must be a list of integers.
* `_num_relays`: The number of relays in the network. This must be a list of integers.
* `_topology_type`: The type of topology to use. This must be a list of strings. Valid values are:
  * `erdos renyi`: An Erdos-Renyi topology.
  * `small world`: A small-world topology.
  * `ring`: A ring topology.
  * `star`: A star topology.
  * `clique`: A clique topology.
  * `custom`: A custom topology. To define a custom topology, you must also specify the `_topology_edges` key.
* `_topology_edges`: The edges of the custom topology. This is defined as a list of lists of 5-tuples, where each
  5-tuple contains the following information:
  1. The source node ID.
  2. The destination node ID.
  3. The combined latency of the edge, in milliseconds, or -1 if unconstrained.
  4. The combined download bandwidth of the edge, in bytes per second, or -1 if unconstrained.
  5. The combined upload bandwidth of the edge, in bytes per second, or -1 if unconstrained.

Consider the following example, with one isolated relay node and two directly-connected producer nodes:

    _num_producers: [ 2 ]
    _num_relays: [ 1 ]
    _topology_type: [ "custom" ]
    _topology_edges: [
      [
        [ 0, 1, 20, 1000000, 1000000 ],
        [ 1, 0, 20, 1000000, 1000000 ]
      ]
    ]

### Running the Compiler

Once you have created the experiment definition file, you can run the compiler to generate a set of experiments:

    $ source venv/bin/activate
    $ python3.11 compiler.py --experiment-definition <experiment-definition-file> --output-dir <output-directory>

The compiler will generate the requested set of experiments in the output directory. Each experiment is YAML file
containing all the information needed to execute that simulation. As these experiments will be evaluated by the
local testnet simulator, the YAML files are compatible with the simulator's experiment definition format.

For more information on how to run the local testnet simulator,
see the [local simulator guide](../../LocalTestnetSimulatorGuide.md).
