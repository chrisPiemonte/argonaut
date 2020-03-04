import networkx as nx
from pathlib import Path
import matplotlib.pyplot as plt
import crawler.text.TextAnalyzer
import crawler.utils.io as io
from crawler.utils.twitter_utils import *
import crawler.utils.common_utils as utils
from crawler.argumentation.convert import common
from crawler.argumentation.convert import to_prolog


def get_edge_weight(similarity, sentiment, other_sentiment):
    return similarity * sentiment * other_sentiment

def draw_graph(G, with_labels=False, node_shape='o', node_color='#B71C1C',
               edge_color='#455A64', node_size=100, width=1, edge_labels=True):
    pos = nx.spring_layout(G)
    draw_options = {
        'with_labels': with_labels,
        'node_shape' : node_shape,
        'node_color' : node_color,
        'edge_color' : edge_color,
        'node_size'  : node_size,
        'width'      : width
    }
    if edge_labels:
        # TODO: wtf
        edge_labels = nx.get_edge_attributes(G, 'weight')
        nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_color='#006064');
    else:
        nx.draw(G, pos, **draw_options)

def remove_nones(Graph):
    try:
        Graph.remove_node(None)
    except Exception as e:
        pass
        # print(e)

def merge_multiedges(MultiDiGraph):
    Graph = nx.DiGraph()
    for u, v, data in MultiDiGraph.edges(data=True):
        Graph.add_node(u)
        Graph.add_node(v)
        # COPY ATTRIBUTES
        for attribute in MultiDiGraph.node[u]:
            Graph.node[u][attribute] = MultiDiGraph.node[u][attribute]
        for attribute in MultiDiGraph.node[v]:
            Graph.node[v][attribute] = MultiDiGraph.node[v][attribute]
        w = data['weight'] if 'weight' in data else 0.0
        if Graph.has_edge(u, v):
            Graph[u][v]['weight'] += w
            Graph[u][v]['num'] += 1.0
        else:
            Graph.add_edge(u, v, weight=w, num=1.0)
    for u, v, data in Graph.edges(data=True):
        if 'num' in data:
            Graph[u][v]['weight'] /= Graph[u][v]['num']
    return Graph

def count_nodes(Graph):
    return len(set(Graph.nodes()))

def count_edges(Graph):
    # print(sorted(Graph.edges(data=True)))
    return len(set(Graph.edges()))

def count_edges_with_zero_weight(Graph, n_decimal=2):
    return len([data['weight'] for _s, _d, data in Graph.edges(data=True) if __is_zero_weight(data['weight'], n_decimal)])

def __is_zero_weight(weight, n_decimal=2):
    return weight == 0 or weight != weight or round(weight, n_decimal) == 0
