import argonaut.utils.common_utils as utils
from argonaut.argumentation.convert.frameworks import bwaf, waf, baf, af
from argonaut.argumentation.convert import common

# ACCEPTED_FRAMEWORKS = ['af', 'baf', 'waf', 'bwaf']

def node_to_argument(node):
    return f'argument({str(node)}).'

def edge_to_relationship(source, dest, weight, framework=common.BWAF, n_decimal=2):
    assert framework in common.ACCEPTED_FRAMEWORKS
    weight = round(weight, n_decimal)
    relationship = ''
    if framework == common.BWAF:
        relationship = bwaf.edge_to_relationship(source, dest, weight)
    elif framework == common.BAF:
        relationship = baf.edge_to_relationship(source, dest, weight)
    elif framework == common.WAF:
        relationship = waf.edge_to_relationship(source, dest, weight)
    elif framework == common.AF:
        relationship = af.edge_to_relationship(source, dest, weight)
    return relationship

def edge_to_rel_weight(source, dest, weight, framework=common.BWAF, n_decimal=2):
    assert framework in common.ACCEPTED_FRAMEWORKS
    weight = round(weight, n_decimal)
    rel_weight   = ''
    if framework == common.BWAF:
        rel_weight   = bwaf.edge_to_rel_weight(source, dest, weight)
    elif framework == common.BAF:
        rel_weight   = baf.edge_to_rel_weight(source, dest, weight)
    elif framework == common.WAF:
        rel_weight   = waf.edge_to_rel_weight(source, dest, weight)
    elif framework == common.AF:
        rel_weight   = af.edge_to_rel_weight(source, dest, weight)
    return rel_weight

def to_facts(Graph, framework=common.BWAF, n_decimal=2, verbose=True):
    assert framework in common.ACCEPTED_FRAMEWORKS
    arguments_set     = set()
    relationships_set = set()
    rel_weights_set   = set()
    for source, dest, data in Graph.edges(data=True):
        weight = data['weight'] if 'weight' in data else 0.0
        source_argument = node_to_argument(source)
        dest_argument   = node_to_argument(dest)
        relationship    = edge_to_relationship(source, dest, weight, framework=framework, n_decimal=n_decimal)
        rel_weight      = edge_to_rel_weight(source, dest, weight, framework=framework, n_decimal=n_decimal)

        if weight != 0:
            if framework == common.BWAF:
                arguments_set.add(source_argument)
                arguments_set.add(dest_argument)
                relationships_set.add(relationship)
                rel_weights_set.add(rel_weight)
            elif framework == common.BAF:
                arguments_set.add(source_argument)
                arguments_set.add(dest_argument)
                relationships_set.add(relationship)
            elif framework == common.WAF:
                if round(weight, n_decimal) < 0:
                    arguments_set.add(source_argument)
                    arguments_set.add(dest_argument)
                    relationships_set.add(relationship)
                    rel_weights_set.add(rel_weight)
            elif framework == common.AF:
                if round(weight, n_decimal) < 0:
                    arguments_set.add(source_argument)
                    arguments_set.add(dest_argument)
                    relationships_set.add(relationship)
    if verbose:
        print(f'MINED {framework} FROM GRAPH.')
        print(f'MINED {len(arguments_set)} ARGUMENTS.')
        print(f"""MINED {len(relationships_set)} RELATIONSHIPS
                  OF WHICH {count_facts(relationships_set, of_type='attack')} ATTACKS
                  and {count_facts(relationships_set, of_type='support')} SUPPORTS.""")
        print(f'MINED {len(rel_weights_set)} ARGUMENTS.')

    return arguments_set.union(relationships_set).union(rel_weights_set)


def count_facts(facts, of_type=''):
    return len([fact for fact in facts if fact.startswith(of_type)])
