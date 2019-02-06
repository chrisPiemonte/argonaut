import numpy as np
from pathlib import Path
import tweepy, gensim, nltk, yaml, os, sys
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# PATHS
ROOT_PATH   = Path(Path(__file__).parent.parent.parent.parent)
RES_PATH    = Path(ROOT_PATH, 'res')
DATA_PATH   = Path(ROOT_PATH, 'data')
MODELS_PATH = Path(ROOT_PATH, 'models')
PRETRAINED_MODELS_PATH = Path(MODELS_PATH, 'pretrained')

CREDENTIALS_PATH = Path(RES_PATH, 'credentials.yml')
W2V_GOOGLENEWS_MODEL_PATH  = Path(PRETRAINED_MODELS_PATH, 'GoogleNews-vectors-negative300.bin.gz')

# URLS
W2V_GOOGLENEWS__MODEL_URL = 'https://s3.amazonaws.com/dl4j-distribution/GoogleNews-vectors-negative300.bin.gz'

class Credentials:
    def __init__(self, credential_file):
        self.cred = yaml.load(open(credential_file))
        self.reddit  = self.cred['reddit']
        self.twitter = self.cred['twitter']


class Comment:
    sia = SentimentIntensityAnalyzer()

    def __init__(self, id, parent, parent_user, text='', parent_text='', user=''):
        self.id    = id
        self.text  = text
        self.user  = user
        self.parent      = parent
        self.parent_user = parent_user
        self.parent_text = parent_text
        self.sentiment   = Comment.get_sentiment(text)

    def __repr__(self):
        return 'Comment({}, {}, {})'.format(self.id, self.parent, self.user)

    @staticmethod
    def get_sentiment(text):
        return Comment.sia.polarity_scores(text)['compound']

    def get_similarity(self, other):
        # text_source_avg_vector = avg_sentence_vector(conv[i].text.split(),   model=model)
        # text_dest_avg_vector   = avg_sentence_vector(conv[i+1].text.split(), model=model)
        # # similarity = cosine_similarity(text_source_avg_vector, text_dest_avg_vector)
        # similarity = distance.euclidean(text_source_avg_vector.reshape(-1, 1), text_dest_avg_vector.reshape(-1, 1)) * 100
        return 1.0


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
