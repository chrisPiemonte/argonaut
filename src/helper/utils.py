# import networkx as nx
import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
# from nltk.corpus import stopwords
# from nltk.classify import SklearnClassifier
# import matplotlib.pyplot as plt
from nltk.sentiment.vader import SentimentIntensityAnalyzer

class Credentials:
    def __init__(self, credential_file):
        self.cred = yaml.load(open(credential_file))
        self.reddit  = self.cred['reddit']
        self.twitter = self.cred['twitter']

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

    def get_similarity(self, other):
        # text_source_avg_vector = avg_sentence_vector(conv[i].text.split(),   model=model)
        # text_dest_avg_vector   = avg_sentence_vector(conv[i+1].text.split(), model=model)
        # # similarity = cosine_similarity(text_source_avg_vector, text_dest_avg_vector)
        # similarity = distance.euclidean(text_source_avg_vector.reshape(-1, 1), text_dest_avg_vector.reshape(-1, 1)) * 100
        return 1.0


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
        self.sentiment    = Tweet.get_sentiment(text)

    def __repr__(self):
        return 'Comment({}, {}, {})'.format(self.id, self.parent, self.user)

    @staticmethod
    def get_sentiment(text):
        return Tweet.sia.polarity_scores(text)['compound']

    def get_similarity(self, other):
        # text_source_avg_vector = avg_sentence_vector(conv[i].text.split(),   model=model)
        # text_dest_avg_vector   = avg_sentence_vector(conv[i+1].text.split(), model=model)
        # # similarity = cosine_similarity(text_source_avg_vector, text_dest_avg_vector)
        # similarity = distance.euclidean(text_source_avg_vector.reshape(-1, 1), text_dest_avg_vector.reshape(-1, 1)) * 100
        return 1.0
    
    
    
########################################################################################
#### REDDIT #### REDDIT #### REDDIT #### REDDIT #### REDDIT #### REDDIT #### REDDIT ####
########################################################################################
    
def getSubComments(comment, allComments, verbose=True):
    allComments.append(comment)
    if not hasattr(comment, "replies"):
        replies = comment.comments()
        if verbose: print("fetching (" + str(len(allComments)) + " comments fetched total)")
    else:
        replies = comment.replies
    for child in replies:
        getSubComments(child, allComments, verbose=verbose)

def getAll(r, submissionId, verbose=True):
    submission = r.submission(submissionId)
    submission.comments.replace_more(limit=None)
    comments = submission.comments
    commentsList = []
    for comment in comments:
        getSubComments(comment, commentsList, verbose=verbose)
    return commentsList

def to_comments(commentsList):
    return [Comment(comment.id, comment.parent().id, get_author(comment.parent()), text=comment.body, user=get_author(comment)) 
            for comment 
            in commentsList]

def get_author(comment):
    author = None
    try:
        author = comment.author.name
    except:
        pass
    return author
