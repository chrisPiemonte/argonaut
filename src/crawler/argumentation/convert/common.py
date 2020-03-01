
AF   = 'af'
BAF  = 'baf'
WAF  = 'waf'
BWAF = 'bwaf'

ACCEPTED_FRAMEWORKS     = [AF, BAF, WAF, BWAF]

ARGUMENT_RELATIONSHIP   = 'argument'
ATTACK_RELATIONSHIP     = 'attack'
SUPPORT_RELATIONSHIP    = 'support'
REL_WEIGHT_RELATIONSHIP = 'rel_weight'

def remove_blanks(facts):
    facts.discard('')
