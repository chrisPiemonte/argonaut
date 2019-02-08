import networkx as nx
from pathlib import Path
import matplotlib.pyplot as plt
import argonaut.text.TextAnalyzer
import argonaut.utils.common_utils as utils
from argonaut.utils.twitter_utils import *
from argonaut.argumentation.convert import to_prolog


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

def save_graph(Graph, suffix, path=None, framework='bwaf', n_decimal=2):
    graph_name = utils.get_graph_name(suffix=suffix)
    graph_output_path = Path(utils.INTERIM_DATA_PATH, graph_name) if path is None else path + '_graph.pickle'
    utils.pickle_graph(Graph, graph_output_path)
    # save prolog facts
    facts = to_prolog.to_facts(Graph, framework=framework, n_decimal=n_decimal)
    facts_name = utils.get_facts_name(graph_name=graph_name, framework=framework)
    facts_output_path = Path(utils.PROLOG_DATA_PATH, facts_name) if path is None else path + '_facts.pl'
    utils.save_facts(facts, facts_output_path)
    print('Everything saved successfully.')
