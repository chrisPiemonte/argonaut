
def edge_to_relationship(source, dest, weight):
    relationship = ''
    if weight > 0:
        relationship = f'support({source}, {dest}).'
    elif weight < 0:
        relationship = f'attack({source}, {dest}).'
    return relationship

def edge_to_rel_weight(source, dest, weight):
    rel_weight = ''
    if weight > 0:
        rel_weight = f'rel_weight({source}, {dest}, {weight}).'
    elif weight < 0:
        rel_weight = f'rel_weight({source}, {dest}, {weight * -1}).'
    return rel_weight
