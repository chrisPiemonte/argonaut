import sys, os
import networkx as nx
from .common_utils import *
from stackapi import StackAPI
from bs4 import BeautifulSoup

QUESTIONS_URL = 'questions'
QUESTION_URL  = 'questions/%s'
ANSWERS_TO    = 'questions/%s/answers'
COMMENTS_TO   = 'answers/%s/comments'

def get_question_id(question):
    return str(question['question_id'])

def get_answer_id(answer):
    return str(answer['answer_id'])

def get_comment_id(comment):
    return str(comment['comment_id'])

def get_user_id(element):
    return element['owner']['user_id']

def get_text(element):
    html  = element['body']
    bsoup = BeautifulSoup(html, 'html.parser')
    return bsoup.get_text()
