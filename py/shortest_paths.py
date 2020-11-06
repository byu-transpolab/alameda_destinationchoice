import sys
import os

import osmnx as ox
import networkx as nx
import math

import numpy as np
import pandas as pd
import multiprocessing as mp
from itertools import repeat
import time

GRAPH_FILE = "graph.graphml"

def get_shortest_paths(df, place):
    """
    :param df:
    :return: dictionary
    """
    park_points = pd.read_csv("data/park_points.csv")
    # TODO: remove sampling
    park_sample_ids = park_points.sample(20).id
    park_points.set_index('id', inplace=True)
    park_points = park_points.loc[park_sample_ids]
    park_ids = park_points.index.unique()

    # get graph information
    print("Getting graph information from file")
    # project to specified CRS and simplify
    graph = ox.load_graphml(GRAPH_FILE)
    G = ox.project_graph(graph)

    # output file
    write_file = "data/shortest_paths_" + str(os.getpid()) + ".csv"
    f = open(write_file, "w+")
    f.write("geoid, park_id, distance, euc_dist\n")

    i = 1
    for bg in df.itertuples():
        # update progress
        location = (i - 1) / len(df) * 100
        sys.stdout.flush()
        sys.stdout.write("Getting block group %d, %d%% completed \r" % (i, location))
        i += 1

        source = ox.get_nearest_node(graph, (bg.LATITUDE, bg.LONGITUDE))
        # loop through parks
        for park in park_ids:
            these_points = park_points.loc[[park]]
            min_euc = float("inf")

            for point in these_points.itertuples():
                # find closest park point by Euclidean distance in projected coordinates
                dx = point.X - bg.X
                dy = point.Y - bg.Y
                euc_dist = math.sqrt(dx**2 + dy**2)

                if euc_dist < min_euc:
                    min_euc = euc_dist
                    closest_lat = point.LATITUDE
                    closest_lon = point.LONGITUDE

            try:
                target = ox.get_nearest_node(graph, (closest_lat, closest_lon))
                length = nx.shortest_path_length(G, source, target, weight='length')
            except:
                length = float("inf")

            # write out the length between the block group and this park
            f.write(str(bg.Index) + ", " + str(park) + ", " + str(length) + ", " + str(min_euc) + "\n")
            f.flush()
            os.fsync(f.fileno())

    # close the buffer
    f.close()


def get_graph(place, mode):
    """
    Get a graph for the place and write both the original graph and the projected directed
    graph to file
    :param place:
    :param mode: One of "drive", "walk", etc.
    :return: OSMNX projected graph
    """

    # Get the graph and make it a non multigraph
    graph = ox.graph_from_place(place, network_type=mode)
    print("Writing file to " + GRAPH_FILE)
    ox.save_graphml(graph)


if __name__ == "__main__":
    try:
        place = sys.argv[1]
    except ValueError:
        place = None

    if place is None:
        print("======== SHORTEST PATHS CALCLULATOR ===========")
        n_cores = mp.cpu_count()
        print("Executing on " + str(n_cores) + " threads")
        # Read block group / tract centroid information
        tracts = pd.read_csv("data/bg_centroids.csv")
        tracts.set_index('geoid', inplace=True)

        tracts.sample(20) # comment for production
        print("Number of origin points: " + str(len(tracts.index)))

        bg_split = np.array_split(tracts, n_cores)

        a = time.process_time()
        pool = mp.Pool(processes = n_cores)
        pool.starmap(get_shortest_paths, zip(bg_split, repeat(place)))
        b = time.process_time()
        print("Execution time: " + str(b-a))

    else:
        print("======== Fetching Graph ===========")
        print("Location: " + place)
        get_graph(place, "walk")

    sys.stdout.write("\n ====== Finished ======\n")
