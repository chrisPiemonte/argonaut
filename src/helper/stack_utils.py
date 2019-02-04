import sys, os
import networkx as nx
# import matplotlib.pyplot as plt
from stackapi import StackAPI
from bs4 import BeautifulSoup

from .utils import *



questions_url = 'questions'
answers_to    = 'questions/%s/answers'
comments_to   = 'answers/%s/comments'

#
def get_question_id(question):
    return str(question['question_id'])

#
def get_answer_id(answer):
    return str(answer['answer_id'])

#
def get_comment_id(comment):
    return str(comment['comment_id'])

#
def get_user_id(element):
    return element['owner']['user_id']

#
def get_similarity(element, other_element):
    return 1.0

#
def get_sentiment_score(text, other_text):
    return 1.0

#
def get_weight(sentiment, similarity):
    return 1.0

#
def get_sentiment(text):
    return 1.0

#
def get_text(element):
    html  = element['body']
    bsoup = BeautifulSoup(html, 'html.parser')
    return bsoup.get_text()

# 
def get_questions(num_questions=1, site=None, order='desc', sort='votes', filter='withbody'):
    # questions_url = 'questions'
    site.page_size = num_questions
    return site.fetch(questions_url, order=order, sort=sort, filter=filter)

# 
def get_answers(question_id, site=None, order='desc', sort='votes', filter='withbody'):
    # answers_to    = 'questions/%s/answers'
    return site.fetch(answers_to % question_id, order=order, sort=sort, filter=filter)

# 
def get_comments(answer_id, site=None, order='desc', sort='votes', filter='withbody'):
    # comments_to   = 'answers/%s/comments'
    return site.fetch(comments_to % answer_id, order='desc', sort='votes', filter='withbody')