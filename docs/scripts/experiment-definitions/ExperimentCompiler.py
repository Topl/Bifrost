import pathlib

import click
import yaml
import networkx
import itertools

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
def main(experiment_file: pathlib.Path, output_dir: pathlib.Path = None):
    # ensure that the output directory exists
    if output_dir is None:
        output_dir = experiment_file.parent / 'generated-experiments'
    output_dir.mkdir(parents=True, exist_ok=True)

    # load the experiment file from the given path
    experiment = dict()
    with experiment_file.open('r') as f:
        experiment = yaml.safe_load(f)

    def _cartesian_dict(dict_of_options):
        return list(dict(zip(dict_of_options, x)) for x in itertools.product(*dict_of_options.values()))

    # generate a set of jobs from the experiment file, as a cartesian product of all the options
    jobs = list()
    option_sets = _cartesian_dict(experiment)


    # from each job, generate a corresponding experiment file



    pass

def job_to_config(job: dict) -> dict:
    # we take the job format, which comes from the generator, and adapt it to the config format

    # from the provided topology info, build a graph object
    if "topology" not in job:
        # when no topology is provided, assume fully connected clique
        job["topology"] = "clique"

    num_nodes = job["num_producers"] + job["num_relays"]
    match job["topology"]:
        case "clique":
            topology = networkx.complete_graph(num_nodes)
        case "star":
            topology = networkx.star_graph(num_nodes - 1)  # star graph has +1 node in networkx
        case "ring":
            topology = networkx.cycle_graph(num_nodes)
        case "small world":
            topology = networkx.watts_strogatz_graph(num_nodes, 4, 0.1)
        case "erdos renyi":
            topology = networkx.erdos_renyi_graph(num_nodes, 0.4)
        case "custom":
            topology = networkx.DiGraph()
            topology.add_nodes_from(range(num_nodes))
            edge_attributes = dict()
            for each_edge in job["topology_edges"]:
                topology.add_edge(each_edge[0], each_edge[1])
                edge_attributes[(each_edge[0], each_edge[1])]["latency"] = each_edge[2]
                edge_attributes[(each_edge[0], each_edge[1])]["download"] = each_edge[3]
                edge_attributes[(each_edge[0], each_edge[1])]["upload"] = each_edge[4]
            networkx.set_edge_attributes(topology, edge_attributes)


        case _:
            raise ValueError(f"Unknown topology type: {job['topology']}")

    # ensure the topology is a DiGraph
    topology = networkx.DiGraph(topology)


if __name__ == '__main__':
    main()
