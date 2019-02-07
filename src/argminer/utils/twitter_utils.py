import numpy as np
from .common_utils import *
import tweepy, gensim, nltk, yaml, os, sys
from nltk.sentiment.vader import SentimentIntensityAnalyzer

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

def is_response(status):
    return status._json['in_reply_to_user_id'] is not None

def get_num_users(conversation):
    return len({tweet.user for tweet in conversation})

def has_common_user(conv1, conv2):
    common = False
    for tweet in conv1:
        for other_tweet in conv2:
            if tweet.user == other_tweet.user:
                common = True
    return common

def has_common_tweet(conv1, conv2):
    common = False
    for tweet in conv1:
        for other_tweet in conv2:
            if tweet.id == other_tweet.id:
                common = True
    return common

def pop_by_index(index, lista):
    retlist = None, []
    if index < len(lista) - 1:
        retlist = lista[index], lista[:index] + lista[index+1:]
    elif index == len(lista)-1:
        retlist = lista[index], lista[:-1]
    return retlist

def join_conv(index, conversations, has_common):
    new_conversations = conversations.copy()
    current_conv, rest = pop_by_index(index, new_conversations)
    if current_conv:
        for i, conv in enumerate(rest):
            if has_common(current_conv, conv):
                rest[i] = conv + current_conv
                new_conversations = rest + [[]]
                break
    return new_conversations

def merge_conversations(conversations, has_common=has_common_user):
    new_conversations = conversations.copy()
    for i, _ in enumerate(new_conversations):
        new_conversations = join_conv(i, new_conversations, has_common)
    return [conv for conv in new_conversations if conv]  # filter empty lists
