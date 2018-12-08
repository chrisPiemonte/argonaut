import networkx as nx
import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
from nltk.corpus import stopwords
from nltk.classify import SklearnClassifier
import matplotlib.pyplot as plt
from nltk.sentiment.vader import SentimentIntensityAnalyzer

class TwitterAccess:
    def __init__(self, credential_file):
        self.cred = yaml.load(open(credential_file))
        self.auth = tweepy.OAuthHandler(
            self.cred['twitter']['CONSUMER_KEY'],
            self.cred['twitter']['CONSUMER_SECRET']
        )
        self.auth.set_access_token(
            self.cred['twitter']['ACCESS_TOKEN'],
            self.cred['twitter']['ACCESS_TOKEN_SECRET']
        )
        self.api  = tweepy.API(self.auth)

class Tweet:
    sia = SentimentIntensityAnalyzer()

    def __init__(self, my_id, reply_to, text='', user=''):
        self.my_id = my_id
        self.text  = text
        self.user  = user
        self.reply_to  = reply_to
        self.sentiment = Tweet.get_sentiment(text)

    def __repr__(self):
        return 'Mytweet({}, {}, {})'.format(self.my_id, self.reply_to, self.user)

    @staticmethod
    def get_sentiment(text):
        return Tweet.sia.polarity_scores(text)['compound']


def extend(api, conversation):
    last_tweet = conversation[-1]
    conversation_extended = conversation
    if last_tweet.reply_to is not None:
        try:
            responded_status = api.get_status(last_tweet.reply_to, tweet_mode='extended')
            responded_tweet  = convert(responded_status)
            conversation_extended = extend(api, conversation_extended + [responded_tweet])
        except:
            conversation[-1].reply_to = None
    return conversation_extended

def convert(status):
    return Tweet(
        status._json['id'],
        status._json['in_reply_to_status_id'],
        text=status._json['full_text'],
        user=status._json['user']['id']
    )

def is_response(status):
    return status._json['in_reply_to_user_id'] is not None

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




#############################################################################


def has_common(l1, l2):
    common = False
    for i in l1:
        for j in l2:
            if i.user == j.user:
                common = True
    return common

def pop_by_index(index, lista):
    retlist = None, []
    if index < len(lista)-1:
        retlist = lista[index], lista[:index] + lista[index+1:]
    elif index == len(lista)-1:
        retlist = lista[index], lista[:-1]
    return retlist

def join_elem(elem, lista):
    new_lista = lista.copy()
    for i, l in enumerate(new_lista):
        if has_common(elem, l):
            new_lista[i] = l + elem
            break
    return new_lista

def unify(lista):
    new_list = lista.copy()
    inter = False
    for i, l in enumerate(new_list):
        curr, rest = pop_by_index(i, new_list)
        if any([has_common(curr, r) for r in rest]):
            new_list = join_elem(curr, rest)
            inter = True
    return new_list 
