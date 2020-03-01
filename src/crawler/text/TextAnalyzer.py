import numpy as np
import urllib.request
from pathlib import Path
from scipy import spatial
import tweepy, gensim, nltk, yaml, os, sys
import argonaut.utils.common_utils as utils
from nltk.sentiment.vader import SentimentIntensityAnalyzer

verbose = False

def __get_model(path, url, verbose=False):
    if path.is_file():
        if verbose:
            print('Model present.')
    else:
        print('Model not present, beginning download ...')
        urllib.request.urlretrieve(url, path, utils.__reporthook)
        print('... download finished.')
    if verbose:
        print('Loading the model.')
    return gensim.models.KeyedVectors.load_word2vec_format(str(path), binary=True, limit=50000)

sia   = SentimentIntensityAnalyzer()
model = __get_model(utils.W2V_GOOGLENEWS_MODEL_PATH, utils.W2V_GOOGLENEWS_MODEL_URL, verbose=verbose)

def get_sentiment(sentence):
    return sia.polarity_scores(sentence)['compound']

def get_similarity(sentence, other_sentence):
    sentence_avg_vector = __avg_sentence_vector(sentence.split(), model=model)
    other_sentence_avg_vector = __avg_sentence_vector(other_sentence.split(), model=model)
    similarity = 0.001
    # if both are non all zeroes vectors
    if not(__is_all_zeroes(sentence_avg_vector) or __is_all_zeroes(other_sentence_avg_vector)):
        similarity = __cosine_similarity(sentence_avg_vector, other_sentence_avg_vector)
    return similarity

# function to average all words vectors in a given sentence
def __avg_sentence_vector(sentence, model, num_features=300):
    num_features = model.wv.vectors[0].size
    featureVec = np.zeros((num_features,), dtype="float32")
    nwords = 0
    for word in sentence:
        if word in model.vocab:
            nwords += 1
            featureVec = np.add(featureVec, model[word])
    if nwords > 0:
        featureVec = np.divide(featureVec, nwords)
    return featureVec

def __cosine_similarity(sentence_vector, other_sentence_vector):
    cosine_similarity = 1 - spatial.distance.cosine(sentence_vector, other_sentence_vector)
    return cosine_similarity

def __is_all_zeroes(vector):
    return all(val == 0 for val in vector)
