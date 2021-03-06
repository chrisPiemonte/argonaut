import numpy as np
from pathlib import Path
# from nltk.sentiment.vader import SentimentIntensityAnalyzer
import tweepy, gensim, nltk, yaml, os, sys, pickle, datetime

# PATHS
ROOT_PATH   = Path(Path(__file__).parent.parent.parent.parent)
RES_PATH    = Path(ROOT_PATH, 'res')
DATA_PATH   = Path(ROOT_PATH, 'data')
MODELS_PATH = Path(ROOT_PATH, 'models')
PRETRAINED_MODELS_PATH = Path(MODELS_PATH, 'pretrained')
INTERIM_DATA_PATH = Path(DATA_PATH, 'interim')
PROLOG_DATA_PATH  = Path(DATA_PATH, 'prolog')
OUTPUT_DATA_PATH  = Path(DATA_PATH, 'output')

CREDENTIALS_PATH = Path(RES_PATH, 'credentials.yml')
W2V_GOOGLENEWS_MODEL_PATH = Path(PRETRAINED_MODELS_PATH, 'GoogleNews-vectors-negative300.bin.gz')

# URLS
W2V_GOOGLENEWS_MODEL_URL = 'https://s3.amazonaws.com/dl4j-distribution/GoogleNews-vectors-negative300.bin.gz'

class Credentials:
    def __init__(self, credential_file):
        self.cred = yaml.load(open(credential_file))
        self.reddit  = self.cred['reddit']
        self.twitter = self.cred['twitter']

class Comment:
    def __init__(self, id, parent, parent_user, text='', parent_text='', user=''):
        self.id    = id
        self.text  = text
        self.user  = user
        self.parent      = parent
        self.parent_user = parent_user
        self.parent_text = parent_text
    def __repr__(self):
        return f'Comment({self.id}, {self.user}, {self.parent})'

# callback for showing progress in url download
def __reporthook(blocknum, blocksize, totalsize):
    readsofar = blocknum * blocksize
    if totalsize > 0:
        percent = readsofar * 1e2 / totalsize
        s = "\r%5.1f%% %*d / %d" % (
            percent, len(str(totalsize)), readsofar, totalsize)
        sys.stderr.write(s)
        if readsofar >= totalsize: # near the end
            sys.stderr.write("\n")
    else: # total size is unknown
        sys.stderr.write("read %d\n" % (readsofar,))


# IO
# def pickle_graph(Graph, path):
#     path = Path(path)
#     path.parent.mkdir(parents=True, exist_ok=True)
#     with open(str(path), 'wb') as file:
#         pickle.dump(Graph, file)
#     print(f'Graph pickled successfully at: {path}.', '\n')
# 
# def load_pickled_graph(path):
#     with open(str(path), 'rb') as file:
#         Graph = pickle.load(file)
#     return Graph
# 
# def get_graph_name(suffix=''):
#     return f'{__get_time()}_{suffix}_graph.pickle'
#
# def __get_time(format='%y%m%d-%H%M%S'):
#     return datetime.datetime.now().strftime(format)
#
# def save_facts(facts, path):
#     path = Path(path)
#     path.parent.mkdir(parents=True, exist_ok=True)
#     with open(str(path), 'w') as file:
#         for fact in sorted(facts):
#             file.write(str(fact) + '\n')
#     print(f'Prolog facts saved successfully at: {path}.')
#
# def get_facts_name(suffix='', graph_name=None, framework=''):
#     facts_name = f'{__get_time()}_{suffix}_{framework}_facts.pl'
#     if graph_name is not None:
#         graph_path = Path(graph_name)
#         facts_name = graph_path.name.replace('graph.pickle', f'{framework}_facts.pl')
#     return facts_name
