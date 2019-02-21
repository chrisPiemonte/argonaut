import argonaut.utils.io as io
import argonaut.utils.common_utils as utils


def user_nodes_to_lines(Graph, sep=','):
    lines = []
    for node_id in Graph.nodes:
        text   = list_to_string(Graph.node[node_id].get('text', ''))
        lines += user_node_to_text(node_id, text, sep=sep)
    return lines

def comment_nodes_to_lines(Graph, sep=','):
    lines = []
    for node_id in Graph.nodes:
        text = Graph.node[node_id].get('text', '')
        user = Graph.node[node_id].get('user', '')
        lines += comment_node_to_text(node_id, text, user, sep=sep)
    return lines

def edges_to_lines(Graph, sep=','):
    lines = []
    for source, dest, data in Graph.edges(data=True):
        weight = str(data['weight']) if 'weight' in data else ''
        lines += edge_to_text(source, dest, weight, sep=sep)
    return lines



def user_node_to_text(node_id, text, sep=','):
    return f'{node_id}{sep}{text}'

def comment_node_to_text(node_id, text, user, sep=','):
    return f'{node_id}{sep}{text}{sep}{user}'

def edge_to_text(source, dest, weight, sep=','):
    return f'{source}{sep}{dest}{sep}{weight}'


def list_to_string(l, sep=' --- '):
    return sep.join(l)
