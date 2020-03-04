import sys, os, praw
import networkx as nx
from bs4 import BeautifulSoup
import crawler.utils.io as io
import crawler.utils.common_utils as utils
from crawler.argumentation.mine.common import *
import crawler.text.TextAnalyzer as TextAnalyzer
from crawler.argumentation.convert import common
import crawler.utils.reddit_utils as reddit_utils
from crawler.argumentation.convert import to_prolog

credentials = utils.Credentials(utils.CREDENTIALS_PATH)
reddit = praw.Reddit(
    client_id=credentials.reddit['CLIENT_ID'],
    client_secret=credentials.reddit['CLIENT_SECRET'],
    user_agent=reddit_utils.USER_AGENT
)

def get_debate_graph(submissionId=None, mode='comments', save=True, path=None,
                     multiedges=False, framework=common.BWAF, n_decimal=2, verbose=False):
    comments = get_comments(submissionId, verbose=verbose)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(comments)
    elif mode == 'users':
        Graph = __build_graph_from_users(comments)
        if not multiedges:
            Graph = merge_multiedges(Graph)
    else:
        raise Exception()
    remove_nones(Graph)
    if save:
        suffix = f'reddit_{mode}'
        io.save_graph(Graph, suffix, path=path, mode=mode, framework=framework, n_decimal=n_decimal, verbose=verbose)
    if verbose:
        print(f'NUMBER OF NODES IN THE GRAPH:      {count_nodes(Graph)}')
        print(f'NUMBER OF EDGES IN THE GRAPH:      {count_edges(Graph)}')
        print(f'NUMBER OF NULL EDGES IN THE GRAPH: {count_edges_with_zero_weight(Graph)}', '\n')
    return Graph

def get_comments(submissionId, verbose=False):
    submission = reddit.submission(submissionId)
    result     = reddit_utils.getAll(reddit, submissionId, verbose=verbose)
    comments   = reddit_utils.to_comments(result)
    if verbose:
        print('SUBMISSION: %s' % submission.title)
        print('URL:        https://www.reddit.com%s' % submission.permalink)
        print('NUMBER OF COMMENTS: %s' % len(comments), '\n')
    return comments

def __build_graph_from_comments(comments):
    Graph = nx.MultiDiGraph()
    for i, comment in enumerate(comments):
        if comment.parent is not None:
            comment_sentiment = TextAnalyzer.get_sentiment(comment.text)
            parent_sentiment  = TextAnalyzer.get_sentiment(comment.parent_text)
            similarity = TextAnalyzer.get_similarity(comment.text, comment.parent_text)
            weight = get_edge_weight(similarity, comment_sentiment, parent_sentiment)
            # ADD NODES ATTRIBUTES
            if comment.id not in Graph.nodes:
                Graph.add_node(comment.id, text=comment.text, user=comment.user)
            if comment.parent not in Graph.nodes:
                Graph.add_node(comment.parent, text=comment.parent_text, user=comment.parent_user)
            # ADD EDGE
            Graph.add_edge(comment.id, comment.parent, weight=weight)
        else:
            pass
    return Graph

def __build_graph_from_users(comments):
    Graph = nx.MultiDiGraph()
    for i, comment in enumerate(comments):
        if comment.parent is not None:
            comment_sentiment = TextAnalyzer.get_sentiment(comment.text)
            parent_sentiment  = TextAnalyzer.get_sentiment(comment.parent_text)
            similarity = TextAnalyzer.get_similarity(comment.text, comment.parent_text)
            weight = get_edge_weight(similarity, comment_sentiment, parent_sentiment)
            # ADD NODES ATTRIBUTES
            if comment.user in Graph.node:
                Graph.node[comment.user]['text'].add(comment.text)
            else:
                Graph.add_node(comment.user, text={comment.text})
            if comment.parent_user in Graph.node:
                Graph.node[comment.parent_user]['text'].add(comment.parent_text)
            else:
                Graph.add_node(comment.parent_user, text={comment.parent_text})
            # ADD EDGE
            Graph.add_edge(comment.user, comment.parent_user, weight=weight)
        else:
            pass
    return Graph
