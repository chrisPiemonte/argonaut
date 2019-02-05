import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# import networkx as nx
# from nltk.corpus import stopwords
# from nltk.classify import SklearnClassifier
# import matplotlib.pyplot as plt

class Credentials:
    def __init__(self, credential_file):
        self.cred = yaml.load(open(credential_file))
        self.reddit  = self.cred['reddit']
        self.twitter = self.cred['twitter']


def avg_sentence_vector(sentence, model, num_features=300):
    #function to average all words vectors in a given sentence
    featureVec = np.zeros((num_features,), dtype="float32")
    nwords = 0

    for word in sentence:
        if word in model.vocab:
            nwords += 1
            featureVec = np.add(featureVec, model[word])

    if nwords > 0:
        featureVec = np.divide(featureVec, nwords)
    return featureVec

cosine_similarity = lambda s1, s2: round(np.dot(s1, s2) / (np.linalg.norm(s1) * np.linalg.norm(s2)), 4)


class Comment:
    sia = SentimentIntensityAnalyzer()

    def __init__(self, id, parent, parent_user, text='', user=''):
        self.id    = id
        self.text  = text
        self.user  = user
        self.parent       = parent
        self.parent_user  = parent_user
        self.sentiment    = Comment.get_sentiment(text)

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
