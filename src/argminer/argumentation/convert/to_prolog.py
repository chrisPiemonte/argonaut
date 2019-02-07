import argminer.utils.common_utils as utils
from argminer.argumentation.convert.frameworks import bwaf, waf, baf, af

ACCEPTED_FRAMEWORKS = ['af', 'baf', 'waf', 'bwaf']

def node_to_argument(node):
    return f'argument({str(node)}).'

def edge_to_relationship(source, dest, weight, framework='bwaf', n_decimal=2):
    assert framework in ACCEPTED_FRAMEWORKS
    weight = round(weight, n_decimal)
    relationship = ''
    if framework == 'bwaf':
        relationship = bwaf.edge_to_relationship(source, dest, weight)
    elif framework == 'baf':
        relationship = baf.edge_to_relationship(source, dest, weight)
    elif framework == 'waf':
        relationship = waf.edge_to_relationship(source, dest, weight)
    elif framework == 'af':
        relationship = af.edge_to_relationship(source, dest, weight)
    return relationship

def edge_to_rel_weight(source, dest, weight, framework='bwaf', n_decimal=2):
    assert framework in ACCEPTED_FRAMEWORKS
    weight = round(weight, n_decimal)
    rel_weight   = ''
    if framework == 'bwaf':
        rel_weight   = bwaf.edge_to_rel_weight(source, dest, weight)
    elif framework == 'baf':
        rel_weight   = baf.edge_to_rel_weight(source, dest, weight)
    elif framework == 'waf':
        rel_weight   = waf.edge_to_rel_weight(source, dest, weight)
    elif framework == 'af':
        rel_weight   = af.edge_to_rel_weight(source, dest, weight)
    return rel_weight


def to_facts(Graph, framework='bwaf', n_decimal=2):
    assert framework in ACCEPTED_FRAMEWORKS
    arguments_set     = set()
    relationships_set = set()
    rel_weights_set   = set()
    for source, dest, data in Graph.edges(data=True):
        weight = data['weight'] if 'weight' in data else 0.0
        source_argument = node_to_argument(source)
        dest_argument   = node_to_argument(dest)
        relationship    = edge_to_relationship(source, dest, weight, framework=framework, n_decimal=n_decimal)
        rel_weight      = edge_to_rel_weight(source, dest, weight, framework=framework, n_decimal=n_decimal)

        if framework == 'bwaf':
            arguments_set.add(source_argument)
            arguments_set.add(dest_argument)
            relationships_set.add(relationship)
            rel_weights_set.add(rel_weight)
        elif framework == 'baf':
            arguments_set.add(source_argument)
            arguments_set.add(dest_argument)
            relationships_set.add(relationship)
        elif framework == 'waf':
            if round(weight, n_decimal) < 0:
                arguments_set.add(source_argument)
                arguments_set.add(dest_argument)
                relationships_set.add(relationship)
                rel_weights_set.add(rel_weight)
        elif framework == 'af':
            if round(weight, n_decimal) < 0:
                arguments_set.add(source_argument)
                arguments_set.add(dest_argument)
                relationships_set.add(relationship)

    return arguments_set.union(relationships_set).union(rel_weights_set)
