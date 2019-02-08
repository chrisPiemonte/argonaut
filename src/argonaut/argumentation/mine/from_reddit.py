import sys, os, praw
import networkx as nx
from bs4 import BeautifulSoup
import argonaut.utils.common_utils as utils
from argonaut.argumentation.mine.common import *
import argonaut.text.TextAnalyzer as TextAnalyzer
import argonaut.utils.reddit_utils as reddit_utils
from argonaut.argumentation.convert import to_prolog

credentials = Credentials(utils.CREDENTIALS_PATH)
reddit = praw.Reddit(
    client_id=credentials.reddit['CLIENT_ID'],
    client_secret=credentials.reddit['CLIENT_SECRET'],
    user_agent=reddit_utils.USER_AGENT
)

def get_debate_graph(submissionId=None, mode='comments', save=True, path=None, multiedges=False, framework='bwaf', n_decimal=2):
    comments = get_comments(submissionId)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(comments)
    elif mode == 'users':
        Graph = __build_graph_from_users(comments)
    else:
        raise Exception()
    remove_nones(Graph)
    if not multiedges:
        Graph = merge_multiedges(Graph)
    if save:
        suffix = f'reddit_{mode}'
        save_graph(Graph, suffix, path=path, framework=framework, n_decimal=n_decimal)
    return Graph

def get_comments(submissionId):
    submission = reddit.submission(submissionId)
    result     = reddit_utils.getAll(reddit, submissionId, verbose=False)
    comments   = reddit_utils.to_comments(result)
    print('SUBMISSION: %s' % submission.title)
    print('URL:        https://www.reddit.com%s' % submission.permalink)
    print('NUMBER OF COMMENTS: %s' % len(comments))
    return comments

def __build_graph_from_comments(comments):
    Graph = nx.MultiDiGraph()
    for i, comment in enumerate(comments):
        if comment.parent is not None:
            comment_sentiment = TextAnalyzer.get_sentiment(comment.text)
            parent_sentiment  = TextAnalyzer.get_sentiment(comment.parent_text)
            similarity = TextAnalyzer.get_similarity(comment.text, comment.parent_text)
            weight = get_edge_weight(similarity, comment_sentiment, parent_sentiment)
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
            # TODO merge edges
            Graph.add_edge(comment.user, comment.parent_user, weight=weight)
        else:
            pass
    return Graph
