import tweepy
import networkx as nx
import argonaut.utils.io as io
from functools import lru_cache
from argonaut.utils.twitter_utils import *
import argonaut.utils.common_utils as utils
from argonaut.argumentation.mine.common import *
import argonaut.text.TextAnalyzer as TextAnalyzer
from argonaut.argumentation.convert import common
from argonaut.argumentation.convert import to_prolog

credentials = Credentials(utils.CREDENTIALS_PATH)
TWEET_MODE = 'extended'

auth = tweepy.OAuthHandler(
    credentials.twitter['CONSUMER_KEY'],
    credentials.twitter['CONSUMER_SECRET'])
auth.set_access_token(
        credentials.twitter['ACCESS_TOKEN'],
        credentials.twitter['ACCESS_TOKEN_SECRET'])

api = tweepy.API(auth)


def get_debate_graph(query='trump', language='en', mode='comments', save=True, path=None,
                     multiedges=False, framework=common.BWAF, n_decimal=2, verbose=False):
    # It is a list of one conversation actually
    conversations = __build_conversations(query=query, language=language, verbose=verbose)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(conversations)
    elif mode == 'users':
        Graph = __build_graph_from_users(conversations)
        if not multiedges:
            Graph = merge_multiedges(Graph)
    else:
        raise Exception()
    remove_nones(Graph)
    if save:
        suffix = f'twitter_{mode}'
        io.save_graph(Graph, suffix, path=path, mode=mode, framework=framework, n_decimal=n_decimal, verbose=verbose)
    if verbose:
        print(f'NUMBER OF NODES IN THE GRAPH:      {count_nodes(Graph)}')
        print(f'NUMBER OF EDGES IN THE GRAPH:      {count_edges(Graph)}')
        print(f'NUMBER OF NULL EDGES IN THE GRAPH: {count_edges_with_zero_weight(Graph)}', '\n')
    return Graph

@lru_cache(maxsize=None)
def __build_conversations(query='trump', language='en', verbose=False):
    search_options = {
        'q':          query,
        'lang':       language,
        'tweet_mode': TWEET_MODE
    }
    statuses = tweepy.Cursor(api.search, **search_options).items(1000)
    tweets   = [[convert(status)] for status in statuses if is_response(status)]
    convs    = [extend(api, tweet) for tweet in tweets]
    merged_convs = merge_conversations(convs)
    if verbose:
        print('RESPONSE TWEETS:             %s' % len(tweets))
        print('MAX CONVERSATION LENGTH:     %s' % max([len(c) for c in convs]))
        print('MAX USERS IN A CONVERSATION: %s' % max([get_num_users(c) for c in convs]), '\n')
    return merged_convs


def __build_graph_from_comments(conversations):
    conversations = [max(conversations, key=len)]
    Graph = nx.MultiDiGraph()
    for conv in conversations:
        for i, tweet in enumerate(conv):
            if tweet.parent is not None:
                answered_tweet = conv[i+1]
                # COMPUTE THE WEIGHT
                similarity = TextAnalyzer.get_similarity(tweet.text, answered_tweet.text)
                tweet_sentiment = TextAnalyzer.get_sentiment(tweet.text)
                answered_tweet_sentiment = TextAnalyzer.get_sentiment(answered_tweet.text)
                weight = get_edge_weight(similarity, tweet_sentiment, answered_tweet_sentiment)
                # ADD NODES ATTRIBUTES
                Graph.add_node(tweet.id, text=tweet.text, user=tweet.user)
                Graph.add_node(answered_tweet.id, text=answered_tweet.text, user=answered_tweet.user)
                # ADD EDGE
                Graph.add_edge(tweet.id, answered_tweet.id, weight=weight)
            else:
                pass
    return Graph

def __build_graph_from_users(conversations):
    conversations = [max(conversations, key=get_num_users)]
    Graph = nx.MultiDiGraph()
    for conv in conversations:
        for i, tweet in enumerate(conv):
            if tweet.parent is not None:
                answered_tweet = conv[i+1]
                # COMPUTE THE WEIGHT
                similarity = TextAnalyzer.get_similarity(tweet.text, answered_tweet.text)
                tweet_sentiment = TextAnalyzer.get_sentiment(tweet.text)
                answered_tweet_sentiment = TextAnalyzer.get_sentiment(answered_tweet.text)
                weight = get_edge_weight(similarity, tweet_sentiment, answered_tweet_sentiment)
                # ADD NODES ATTRIBUTES
                if tweet.user in Graph.node:
                    Graph.node[tweet.user]['text'].add(tweet.text)
                else:
                    Graph.add_node(tweet.user, text={tweet.text})
                if answered_tweet.user in Graph.node:
                    Graph.node[answered_tweet.user]['text'].add(answered_tweet.text)
                else:
                    Graph.add_node(answered_tweet.user, text={answered_tweet.text})
                # ADD EDGE
                Graph.add_edge(tweet.user, answered_tweet.user, weight=weight)
            else:
                pass
    return Graph
