import unittest
import networkx as nx
from crawler.argumentation.convert import common
from crawler.argumentation.convert.to_prolog import *


class TestToProlog(unittest.TestCase):

    def setUp(self):
        # GRAPH
        self.Graph = nx.MultiDiGraph()
        self.Graph.add_edge(1, 2, weight=-0.30)
        self.Graph.add_edge(2, 1, weight=0.38)
        self.Graph.add_edge(2, 4, weight=0.12)
        self.Graph.add_edge(2, 5, weight=-0.76)
        self.Graph.add_edge(7, 8, weight=0.0)  # should not be added beacuse of weight of 0.0
        self.Graph.add_edge(3, 2, weight=0.99)
        self.Graph.add_edge(4, 5, weight=0.01)
        self.Graph.add_edge(5, 3, weight=0.25)
        self.Graph.add_edge(5, 1, weight=-0.87)
        # FACTS
        self.bwaf_facts = to_facts(self.Graph, framework=common.BWAF, verbose=False)
        self.baf_facts  = to_facts(self.Graph, framework=common.BAF, verbose=False)
        self.waf_facts  = to_facts(self.Graph, framework=common.WAF, verbose=False)
        self.af_facts   = to_facts(self.Graph, framework=common.AF, verbose=False)

    def tearDown(self):
        self.Graph      = ''
        self.bwaf_facts = ''
        self.baf_facts  = ''
        self.waf_facts  = ''
        self.af_facts   = ''

    # ==========================================================================
    # =============================== BWAF =====================================
    # ==========================================================================

    def test_number_of_arguments_from_synthetic_graph_bwaf(self):
        self.assertEqual(count_facts(self.bwaf_facts, of_type=common.ARGUMENT_RELATIONSHIP), 5)

    def test_number_of_attacks_from_synthetic_graph_bwaf(self):
        self.assertEqual(count_facts(self.bwaf_facts, of_type=common.ATTACK_RELATIONSHIP), 3)

    def test_number_of_supports_from_synthetic_graph_bwaf(self):
        self.assertEqual(count_facts(self.bwaf_facts, of_type=common.SUPPORT_RELATIONSHIP), 5)

    def test_number_of_relationships_from_synthetic_graph_bwaf(self):
        num_attacks = count_facts(self.bwaf_facts, of_type=common.ATTACK_RELATIONSHIP)
        num_support = count_facts(self.bwaf_facts, of_type=common.SUPPORT_RELATIONSHIP)
        self.assertEqual(num_attacks + num_support, 8)

    def test_number_of_rel_weights_from_synthetic_graph_bwaf(self):
        self.assertEqual(count_facts(self.bwaf_facts, of_type=common.REL_WEIGHT_RELATIONSHIP), 8)

    # ==========================================================================
    # =============================== BAF ======================================
    # ==========================================================================

    def test_number_of_arguments_from_synthetic_graph_baf(self):
        self.assertEqual(count_facts(self.baf_facts, of_type=common.ARGUMENT_RELATIONSHIP), 5)

    def test_number_of_attacks_from_synthetic_graph_baf(self):
        self.assertEqual(count_facts(self.baf_facts, of_type=common.ATTACK_RELATIONSHIP), 3)

    def test_number_of_supports_from_synthetic_graph_baf(self):
        self.assertEqual(count_facts(self.baf_facts, of_type=common.SUPPORT_RELATIONSHIP), 5)

    def test_number_of_relationships_from_synthetic_graph_baf(self):
        num_attacks = count_facts(self.baf_facts, of_type=common.ATTACK_RELATIONSHIP)
        num_support = count_facts(self.baf_facts, of_type=common.SUPPORT_RELATIONSHIP)
        self.assertEqual(num_attacks + num_support, 8)

    def test_number_of_rel_weights_from_synthetic_graph_baf(self):
        self.assertEqual(count_facts(self.baf_facts, of_type=common.REL_WEIGHT_RELATIONSHIP), 0)

    # ==========================================================================
    # =============================== WAF ======================================
    # ==========================================================================

    def test_number_of_arguments_from_synthetic_graph_waf(self):
        self.assertEqual(count_facts(self.waf_facts, of_type=common.ARGUMENT_RELATIONSHIP), 3)

    def test_number_of_attacks_from_synthetic_graph_waf(self):
        self.assertEqual(count_facts(self.waf_facts, of_type=common.ATTACK_RELATIONSHIP), 3)

    def test_number_of_supports_from_synthetic_graph_waf(self):
        self.assertEqual(count_facts(self.waf_facts, of_type=common.SUPPORT_RELATIONSHIP), 0)

    def test_number_of_relationships_from_synthetic_graph_waf(self):
        num_attacks = count_facts(self.waf_facts, of_type=common.ATTACK_RELATIONSHIP)
        num_support = count_facts(self.waf_facts, of_type=common.SUPPORT_RELATIONSHIP)
        self.assertEqual(num_attacks + num_support, 3)

    def test_number_of_rel_weights_from_synthetic_graph_waf(self):
        self.assertEqual(count_facts(self.waf_facts, of_type=common.REL_WEIGHT_RELATIONSHIP), 3)

    # ==========================================================================
    # ================================ AF ======================================
    # ==========================================================================

    def test_number_of_arguments_from_synthetic_graph_af(self):
        self.assertEqual(count_facts(self.af_facts, of_type=common.ARGUMENT_RELATIONSHIP), 3)

    def test_number_of_attacks_from_synthetic_graph_af(self):
        self.assertEqual(count_facts(self.af_facts, of_type=common.ATTACK_RELATIONSHIP), 3)

    def test_number_of_supports_from_synthetic_graph_af(self):
        self.assertEqual(count_facts(self.af_facts, of_type=common.SUPPORT_RELATIONSHIP), 0)

    def test_number_of_relationships_from_synthetic_graph_af(self):
        num_attacks = count_facts(self.af_facts, of_type=common.ATTACK_RELATIONSHIP)
        num_support = count_facts(self.af_facts, of_type=common.SUPPORT_RELATIONSHIP)
        self.assertEqual(num_attacks + num_support, 3)

    def test_number_of_rel_weights_from_synthetic_graph_af(self):
        self.assertEqual(count_facts(self.af_facts, of_type=common.REL_WEIGHT_RELATIONSHIP), 0)

if __name__ == '__main__':
    pass
