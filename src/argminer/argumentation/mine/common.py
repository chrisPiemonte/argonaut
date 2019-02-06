import networkx as nx
import matplotlib.pyplot as plt
import argminer.utils.common_utils as utils
from argminer.utils.twitter_utils import *
import argminer.text.TextAnalyzer


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
