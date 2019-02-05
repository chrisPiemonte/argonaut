# import networkx as nx
import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
# from nltk.corpus import stopwords
# from nltk.classify import SklearnClassifier
# import matplotlib.pyplot as plt
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from .utils import *

# tweet
def extend(api, conversation):
    last_tweet = conversation[-1]
    conversation_extended = conversation
    if last_tweet.parent is not None:
        try:
            responded_status = api.get_status(last_tweet.parent, tweet_mode='extended')
            responded_tweet  = convert(responded_status)
            conversation_extended = extend(api, conversation_extended + [responded_tweet])
        except:
            conversation[-1].parent = None
    return conversation_extended

def convert(status):
    return Comment(
        status._json['id'],
        status._json['in_reply_to_status_id'],
        status._json['in_reply_to_user_id'],
        text=status._json['full_text'],
        user=status._json['user']['id']
    )

# tweet
def is_response(status):
    return status._json['in_reply_to_user_id'] is not None


# class Tweet:
#     sia = SentimentIntensityAnalyzer()
#
#     def __init__(self, my_id, reply_to, text='', user=''):
#         self.my_id = my_id
#         self.text  = text
#         self.user  = user
#         self.reply_to  = reply_to
#         self.sentiment = Tweet.get_sentiment(text)
#
#     def __repr__(self):
#         return 'Mytweet({}, {}, {})'.format(self.my_id, self.reply_to, self.user)
#
#     @staticmethod
#     def get_sentiment(text):
#         return Tweet.sia.polarity_scores(text)['compound']
#
#     def get_similarity(self, other):
#         # text_source_avg_vector = avg_sentence_vector(conv[i].text.split(),   model=model)
#         # text_dest_avg_vector   = avg_sentence_vector(conv[i+1].text.split(), model=model)
#         # # similarity = cosine_similarity(text_source_avg_vector, text_dest_avg_vector)
#         # similarity = distance.euclidean(text_source_avg_vector.reshape(-1, 1), text_dest_avg_vector.reshape(-1, 1)) * 100
#         return 1.0

# class TwitterAccess:
#     def __init__(self, credential_file):
#         self.cred = yaml.load(open(credential_file))
#         self.auth = tweepy.OAuthHandler(
#             self.cred['twitter']['CONSUMER_KEY'],
#             self.cred['twitter']['CONSUMER_SECRET']
#         )
#         self.auth.set_access_token(
#             self.cred['twitter']['ACCESS_TOKEN'],
#             self.cred['twitter']['ACCESS_TOKEN_SECRET']
#         )
#         self.api  = tweepy.API(self.auth)




# def extendOLD(api, conversation):
#     last_tweet = conversation[-1]
#     conversation_extended = conversation
#     if last_tweet.reply_to is not None:
#         try:
#             responded_status = api.get_status(last_tweet.reply_to, tweet_mode='extended')
#             responded_tweet  = convert(responded_status)
#             conversation_extended = extend(api, conversation_extended + [responded_tweet])
#         except:
#             conversation[-1].reply_to = None
#     return conversation_extended
#
# def convertOLD(status):
#     return Tweet(
#         status._json['id'],
#         status._json['in_reply_to_status_id'],
#         text=status._json['full_text'],
#         user=status._json['user']['id']
#     )
