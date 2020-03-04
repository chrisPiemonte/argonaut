import crawler.utils.common_utils as utils


def user_nodes_to_lines(Graph, sep=','):
    lines = []
    for node_id, data in Graph.nodes(data=True):
        text = set([e.replace('\n', '') for e in data.get('text', '')])
        # print(data, 'user_n_to_lines')
        lines.append([node_id, text])
    return lines

def comment_nodes_to_lines(Graph, sep=','):
    lines = []
    for node_id, data in Graph.nodes(data=True):
        text = data.get('text', '').replace('\n', '')
        user = data.get('user', '')
        lines.append([node_id, text, user])
    return lines

def edges_to_lines(Graph, sep=',', n_decimal=2):
    lines = []
    for source, dest, data in Graph.edges(data=True):
        weight = str(round(data['weight'], n_decimal)) if 'weight' in data else ''
        lines.append([source, dest, weight])
    return lines


# USELESS ?
def user_node_to_text(node_id, text, sep=','):
    return f'{node_id}{sep}{text}'

def comment_node_to_text(node_id, text, user, sep=','):
    return f'{node_id}{sep}{text}{sep}{user}'

def edge_to_text(source, dest, weight, sep=','):
    return f'{source}{sep}{dest}{sep}{weight}'


def list_to_string(l, sep=' --- '):
    return sep.join(l)
