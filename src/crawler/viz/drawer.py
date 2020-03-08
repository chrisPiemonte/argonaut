import networkx as nx

# import types
from matplotlib.axes import SubplotBase
from networkx.classes.graph import Graph

class GraphDrawer:

    def __init__(self, graph: Graph, **kwargs) -> None:
        """[summary]
        
        Arguments:
            graph {Graph} -- [description]
        """
        self.graph = graph
        # Default options
        options = {
            'with_labels': False,
            'node_shape' : 'o',
            'node_color' : '#B71C1C',
            'edge_color' : '#455A64',
            'node_size'  : 200,
            'font_color' : '#006064',
            'width': 1,
            'draw_edge_labels': False
        }
        for option, val in kwargs.items():
            options[option] = val
        self.options = options

    
    def draw(self, ax: SubplotBase, title: str = '') -> None:
        """[summary]
        
        Arguments:
            ax {SubplotBase} -- [description]
        
        Keyword Arguments:
            title {str} -- [description] (default: {''})
        """
        ax.set_title(title, fontsize=16)
        pos = nx.spring_layout(self.graph)
        if self.options['draw_edge_labels']:
            # self.options['edge_labels'] = nx.get_edge_attributes(self.graph, 'weight')
            self.options['edge_labels'] = {edge: round(weight, 2) for edge, weight in nx.get_edge_attributes(self.graph, 'weight').items()}
            nx.draw_networkx_edge_labels(self.graph, pos, ax=ax, **self.options)
            nx.draw(self.graph, pos, ax=ax, **self.options)
        else:
            nx.draw(self.graph, pos, ax=ax, **self.options)
