# import networkx as nx
import numpy as np
import tweepy, gensim, nltk, yaml, os, sys
# from nltk.corpus import stopwords
# from nltk.classify import SklearnClassifier
# import matplotlib.pyplot as plt
from nltk.sentiment.vader import SentimentIntensityAnalyzer

from .utils import *

    
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