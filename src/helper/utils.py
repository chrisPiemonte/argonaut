import networkx as nx
import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
from nltk.corpus import stopwords
from nltk.classify import SklearnClassifier
import matplotlib.pyplot as plt
from nltk.sentiment.vader import SentimentIntensityAnalyzer

cred = yaml.load(open('../res/credentials.yml'))

auth = tweepy.OAuthHandler(cred['twitter']['CONSUMER_KEY'], cred['twitter']['CONSUMER_SECRET'])
auth.set_access_token(cred['twitter']['ACCESS_TOKEN'], cred['twitter']['ACCESS_TOKEN_SECRET'])

api = tweepy.API(auth)

class Tweet:
    sia = SentimentIntensityAnalyzer()
    
    def __init__(self, my_id, reply_to, text=''):
        self.my_id = my_id
        self.reply_to = reply_to
        self.text = text
        self.sentiment = Tweet.get_sentiment(text)
        
    def __repr__(self):
        return 'Mytweet({}, {})'.format(self.my_id, self.reply_to, self.text)
    
    @staticmethod
    def get_sentiment(text):
        return Tweet.sia.polarity_scores(text)['compound']
        

def extend(conversation):
    last_tweet = conversation[-1]
    conversation_extended = conversation
    if last_tweet.reply_to is not None:
        try:
            responded_status = api.get_status(last_tweet.reply_to, tweet_mode='extended')
            responded_tweet  = convert(responded_status)
            conversation_extended = extend(conversation_extended + [responded_tweet])
        except:
            conversation[-1].reply_to = None
    return conversation_extended

def convert(status):
    return Tweet(
        status._json['id'], 
        status._json['in_reply_to_status_id'],
        text=status._json['full_text']
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