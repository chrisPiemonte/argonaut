import tweepy
import networkx as nx
from functools import lru_cache
from argonaut.utils.twitter_utils import *
import argonaut.utils.common_utils as utils
from argonaut.argumentation.mine.common import *
import argonaut.text.TextAnalyzer as TextAnalyzer
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
                     multiedges=False, framework='bwaf', n_decimal=2):
    # It is a list of one conversation actually
    conversations = __build_conversations(query=query, language=language)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(conversations)
    elif mode == 'users':
        Graph = __build_graph_from_users(conversations)
    else:
        raise Exception()
    remove_nones(Graph)
    if not multiedges:
        Graph = merge_multiedges(Graph)
    if save:
        suffix = f'twitter_{mode}'
        # save pickle
        # graph_name  = utils.get_graph_name(suffix=suffix)
        # graph_output_path = Path(utils.INTERIM_DATA_PATH, graph_name) if path is None else path
        # utils.pickle_graph(Graph, graph_output_path)
        # save prolog facts
        # facts = to_prolog.to_facts(Graph, framework=framework, n_decimal=n_decimal)
        # facts_name = utils.get_facts_name(graph_name=graph_name)
        # facts_output_path = Path(utils.PROLOG_DATA_PATH, facts_name) if path is None else path
        # utils.save_facts(facts, facts_output_path)
        save_graph(Graph, suffix, path=path, framework=framework, n_decimal=n_decimal)
    return Graph

@lru_cache(maxsize=None)
def __build_conversations(query='trump', language='en'):
    search_options = {
        'q':          query,
        'lang':       language,
        'tweet_mode': TWEET_MODE
    }
    statuses = tweepy.Cursor(api.search, **search_options).items(1000)
    tweets   = [[convert(status)] for status in statuses if is_response(status)]
    convs    = [extend(api, tweet) for tweet in tweets]
    merged_convs = merge_conversations(convs)
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
                similarity = TextAnalyzer.get_similarity(tweet.text, answered_tweet.text)
                tweet_sentiment = TextAnalyzer.get_sentiment(tweet.text)
                answered_tweet_sentiment = TextAnalyzer.get_sentiment(answered_tweet.text)
                # or if the edge already exist change the weight
                weight = get_edge_weight(similarity, tweet_sentiment, answered_tweet_sentiment)
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
                similarity = TextAnalyzer.get_similarity(tweet.text, answered_tweet.text)
                tweet_sentiment = TextAnalyzer.get_sentiment(tweet.text)
                answered_tweet_sentiment = TextAnalyzer.get_sentiment(answered_tweet.text)
                # TODO: or if the edge already exist change the weight
                weight = get_edge_weight(similarity, tweet_sentiment, answered_tweet_sentiment)
                # TODO: when multiple edges find a way to merge them
                Graph.add_edge(tweet.user, answered_tweet.user, weight=weight)
            else:
                pass
    return Graph
