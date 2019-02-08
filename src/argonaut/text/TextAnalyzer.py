import numpy as np
import urllib.request
from pathlib import Path
from scipy import spatial
import tweepy, gensim, nltk, yaml, os, sys
import argonaut.utils.common_utils as utils
from nltk.sentiment.vader import SentimentIntensityAnalyzer

def __get_model(path, url):
    if path.is_file():
        print('Model present.')
    else:
        print('Model not present, beginning download ...')
        urllib.request.urlretrieve(url, path, utils.__reporthook)
        print('... download finished.')
    print('Loading the model.')
    return gensim.models.KeyedVectors.load_word2vec_format(str(path), binary=True, limit=50000)

sia   = SentimentIntensityAnalyzer()
model = __get_model(utils.W2V_GOOGLENEWS_MODEL_PATH, utils.W2V_GOOGLENEWS_MODEL_URL)

def get_sentiment(text):
    return sia.polarity_scores(text)['compound']

def get_similarity(text, other_text):
    text_avg_vector = __avg_sentence_vector(text.split(), model=model)
    other_text_avg_vector = __avg_sentence_vector(other_text.split(), model=model)
    return __cosine_similarity(text_avg_vector, other_text_avg_vector)

#function to average all words vectors in a given sentence
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

def __cosine_similarity(sentence, other_sentence):
    # return round(np.dot(sentence, other_sentence) / (np.linalg.norm(sentence) * np.linalg.norm(other_sentence)), 4)
    return 1 - spatial.distance.cosine(sentence, other_sentence)
