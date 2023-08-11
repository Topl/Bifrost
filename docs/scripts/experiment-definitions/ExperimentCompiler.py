import functools
import itertools
import operator
import pathlib

import click
import networkx
import yaml


# the purpose of this tool is to take a single experiment definition file and generate all the
# experiments that are defined in it. This is useful for when you want to run a large number of
# experiments with a single command, but don't want to have to write a script to generate the
# experiment files.
@click.command()
@click.argument('experiment_file',
                type=click.Path(exists=True, readable=True, path_type=pathlib.Path),
                required=True)
@click.argument('output_dir',
                type=click.Path(exists=True, writable=True, file_okay=False, path_type=pathlib.Path),
                required=False)
def main(experiment_file: pathlib.Path, output_dir: pathlib.Path = None) -> None:
    # ensure that the output directory exists
    if output_dir is None:
        output_dir = experiment_file.parent / 'generated-experiments'
    output_dir.mkdir(parents=True, exist_ok=True)

    # load the experiment file from the given path
    with experiment_file.open('r') as f:
        experiment = yaml.safe_load(f)

    # first, go through the features and look for dictionaries; we will use these to generate lists of ranges
    sanity_bound = 50000  # don't generate more than 50,000 jobs, probably it's a mistake in the input
    for feature_name, feature_data in experiment.items():
        if isinstance(feature_data, dict):
            assert "min" in feature_data, f"Feature {feature_name} is a dictionary, but does not have a 'min' key"
            assert "max" in feature_data, f"Feature {feature_name} is a dictionary, but does not have a 'max' key"
            assert "step" in feature_data, f"Feature {feature_name} is a dictionary, but does not have a 'step' key"
            if "suffix" not in feature_data:
                feature_data["suffix"] = ""

            # we need to replace this dict with a list of values from "min" to "max" with "step" size
            feature_range = list()
            curr_value = feature_data["min"]
            while curr_value <= feature_data["max"] and sanity_bound > 0:
                if len(feature_data["suffix"]) > 0:
                    feature_range.append(f"{curr_value}{feature_data['suffix']}")
                else:
                    feature_range.append(curr_value)
                curr_value += feature_data["step"]
                sanity_bound -= 1

            # if we hit the sanity bound, we need to stop
            if sanity_bound <= 0:
                raise ValueError("Sanity bound exceeded; you probably have a mistake in your experiment file.")

            # replace the dictionary with the list of values
            experiment[feature_name] = feature_range

    def _cartesian_dict(dict_of_options):
        return list(dict(zip(dict_of_options, x)) for x in itertools.product(*dict_of_options.values()))

    # generate a set of jobs from the experiment file, as a cartesian product of all the options
    option_sets = _cartesian_dict(experiment)

    # from each job, generate a corresponding experiment file
    job_index = 0
    for each_job in option_sets:
        # generate the experiment file
        config = job_to_config(each_job)

        # write the experiment file to the output directory
        with (output_dir / f"job-{job_index}.yaml").open('w') as f:
            yaml.safe_dump(config, f)

        job_index += 1


def job_to_config(job: dict) -> dict:
    # we take the job format, which comes from the generator, and adapt it to the config format
    output_config = dict()

    # let's load the template config file
    assert "_template" in job, "No template file provided, but this is required. " \
                               "Please add a '_template' key to the experiment file with the path to the template file."
    if job["_template"][0] == '/':
        template_path = pathlib.Path(job["_template"])  # absolute path
    else:
        template_path = pathlib.Path(__file__).parent / job["_template"]  # relative path
    if not template_path.exists():
        raise FileNotFoundError(f"Could not find template file at {template_path.absolute()}")
    with template_path.open('r') as f:
        output_config: dict = yaml.safe_load(f)
    if "scenario/randomSeed" in job:
        default_seed = False
        seed = job["scenario/randomSeed"]  # this has to come from the JOB, not the template, so it can iterate
    else:
        default_seed = True
        seed = 0

    # now let's set up the topology
    if "_topology_type" in job:
        if "_num_producers" not in job:
            job["_num_producers"] = 0
        if "_num_relays" not in job:
            job["_num_relays"] = 0
        num_nodes = job["_num_producers"] + job["_num_relays"]
        match job["_topology_type"]:
            case "clique":
                topology = networkx.complete_graph(num_nodes)
            case "star":
                topology = networkx.star_graph(num_nodes - 1)  # star graph has +1 node in networkx
            case "ring":
                topology = networkx.cycle_graph(num_nodes)
            case "small world":
                if default_seed:
                    print("WARNING: Using default seed for small world topology; please provide a scenario/randomSeed "
                          "parameter to ensure reproducibility.")
                topology = networkx.watts_strogatz_graph(num_nodes, 4, 0.1, seed=seed)
            case "erdos renyi":
                if default_seed:
                    print("WARNING: Using default seed for small world topology; please provide a scenario/randomSeed "
                          "parameter to ensure reproducibility.")
                topology = networkx.erdos_renyi_graph(num_nodes, 0.4, seed=seed)
            case "custom":
                assert "_topology_edges" in job, "Custom topology requires _topology_edges to be defined"
                topology = networkx.DiGraph()
                topology.add_nodes_from(range(num_nodes))
                edge_attributes = dict()
                for each_edge in job["_topology_edges"]:
                    topology.add_edge(each_edge[0], each_edge[1])
                    edge_attributes[(each_edge[0], each_edge[1])] = dict()
                    edge_attributes[(each_edge[0], each_edge[1])]["latency"] = each_edge[2]
                    edge_attributes[(each_edge[0], each_edge[1])]["download"] = each_edge[3]
                    edge_attributes[(each_edge[0], each_edge[1])]["upload"] = each_edge[4]
                networkx.set_edge_attributes(topology, edge_attributes)

            case _:
                raise ValueError(f"Unknown topology type: {job['topology']}")

        # ensure the topology is a DiGraph, which we will use for other topology-based checks
        topology = networkx.DiGraph(topology)

        # we are temporarily ignoring all topology checks, but those will go here
        # NYI

    def _smart_split(s: str) -> list:
        # after we split the string, numeric values will be converted to int or float to match YAML assumptions
        return [int(x) if x.isdigit() else float(x) if x.replace('.', '', 1).isdigit() else x for x in s.split('/')]

    def read_dict_path(data_dict: dict, map_list: list):
        return functools.reduce(operator.getitem, map_list, data_dict)

    def write_dict_path(data_dict: dict, map_list: list, val):
        read_dict_path(data_dict, map_list[:-1])[map_list[-1]] = val

    def empty_dict_from_path(dict_path: list) -> dict:
        nested_dict: dict = None  # this is the value at the end of the path
        for i in reversed(dict_path):
            nested_dict = {i: nested_dict}  # coerces the upstream values to dict
        return nested_dict

    # now we need to generate the config file, consisting of the following sections:
    # results, scenario, shared-config, and configs
    #   the results section dictates where the results will be stored
    #   the scenario section dictates experiment parameters
    #   the shared-config section dictates protocol parameters
    #   the configs section dictates the node configurations

    # now, let's go through each entry and see if there is a path in the template file for that entry
    for key, value in job.items():
        # skip the keys that start with an underscore, as those are for internal use
        if key.startswith('_'):
            continue

        key_path = _smart_split(key)
        try:
            # get the value in the config file
            _ = read_dict_path(output_config, key_path)  # we don't actually need the value, just to check if it exists
        except KeyError:
            print(f"Could not find key {key} in template file, skipping")
            continue

        # now, we take the new value that is in the job and replace the value in the config file
        write_dict_path(output_config, key_path, value)

    # now, we need to convert from the graph to the topology format
    if "_topology_type" in job:
        topology_dict = dict()
        output_config.pop("configs", None)  # remove the configs section, as we will be replacing it

        # we need to write the topology
        num_producers, num_relays = job["_num_producers"], job["_num_relays"]
        node_map = dict()  # maps node index to node name

        # the first num_producers nodes are producers, the next num_relays are relays
        curr_node = 0
        for each_producer in range(num_producers):
            # all producers have at minimum their index
            producer_path = f"bifrost/big-bang/local-staker-index".split("/")
            node_dict = empty_dict_from_path(producer_path)
            write_dict_path(node_dict, producer_path, each_producer)
            node_dict["topology"] = {"ingress": [], "egress": []}
            topology_dict["producer" + str(each_producer)] = node_dict
            node_map[curr_node] = "producer" + str(each_producer)
            curr_node += 1

        # the next num_relays nodes are relays
        for each_relay in range(num_relays):
            # all relays have at minimum their index
            relay_path = f"bifrost/big-bang/local-staker-index".split("/")
            node_dict = empty_dict_from_path(relay_path)
            node_dict["topology"] = {"ingress": [], "egress": []}

            # relays don't stake, so their staker index is -1
            write_dict_path(node_dict, relay_path, -1)
            topology_dict["relay" + str(each_relay)] = node_dict
            node_map[curr_node] = "relay" + str(each_relay)
            curr_node += 1

        # now, let's go through all the out-edges to set the egress
        # this is very specific to the current config format, so we will need to change this if we change it later
        for each_edge in topology.out_edges:
            # we also need to see the stats for this edge
            source, target = each_edge
            edge_stats = topology.get_edge_data(source, target)

            def _edge_stats_to_throttle(edge_stats: dict) -> dict:
                throttle = dict()
                if "download" in edge_stats and edge_stats["download"] != -1:
                    throttle["downloadBytesPerSecond"] = edge_stats["download"] // 2
                if "upload" in edge_stats and edge_stats["upload"] != -1:
                    throttle["uploadBytesPerSecond"] = edge_stats["upload"] // 2
                if "latency" in edge_stats and edge_stats["latency"] != -1:
                    throttle["latency"] = str(edge_stats["latency"] // 2) + " milli"
                return throttle

            # every egress edge is a list of dicts, with each dict containing "peer" and (optionally) "throttle"
            # note that we divide limits by 2, because they are added symmetrically on the ingress too
            egress_dict = {"peer": node_map[target]}
            throttle_dict = _edge_stats_to_throttle(edge_stats)
            if len(throttle_dict) > 0:
                egress_dict["throttle"] = throttle_dict
            topology_dict[node_map[source]]["topology"]["egress"].append(egress_dict)

            # every ingress edge is a single optional dict for "throttle", which  is applied to all ingress
            throttle_dict = _edge_stats_to_throttle(edge_stats)
            if len(throttle_dict) > 0:
                ingress_dict = {"throttle": throttle_dict}
                topology_dict[node_map[target]]["topology"]["ingress"] = ingress_dict

        # remove empty ingress and egress from each node
        for each_node in topology_dict.values():
            if len(each_node["topology"]["ingress"]) == 0:
                each_node["topology"].pop("ingress", None)
            if len(each_node["topology"]["egress"]) == 0:
                each_node["topology"].pop("egress", None)

        # now, we need to add the topology to the config
        output_config["configs"] = topology_dict

    return output_config


if __name__ == '__main__':
    main()
