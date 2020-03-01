import numpy as np
from .common_utils import *
import tweepy, gensim, nltk, yaml, os, sys
# from nltk.sentiment.vader import SentimentIntensityAnalyzer

USER_AGENT = 'linux:com.example.argumentation:v0.0.1 (by /u/anzianotti)'

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
    return [Comment(
                comment.id,
                comment.parent().id,
                get_author(comment.parent()),
                text=get_text(comment),
                parent_text=get_text(comment.parent()),
                user=get_author(comment))
            for comment
            in commentsList]

def get_author(comment):
    author = None
    try:
        author = comment.author.name
    except:
        pass
    return author

def get_text(comment):
    text = None
    try:
        text = comment.body
    except:
        text = comment.selftext
    return text
