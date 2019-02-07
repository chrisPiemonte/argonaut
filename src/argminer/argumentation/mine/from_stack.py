import sys, os
import networkx as nx
from random import randint
from stackapi import StackAPI
import argminer.utils.common_utils as utils
from argminer.argumentation.mine.common import *
import argminer.utils.stack_utils as stack_utils
import argminer.text.TextAnalyzer as TextAnalyzer

site = StackAPI('stackoverflow')

def get_debate_graph(question=None, mode='comments', save=True, path=None):
    questions_request = stack_utils.QUESTION_URL % question if question is not None else stack_utils.QUESTIONS_URL
    questions = get_questions(questions=questions_request, site=site)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(questions)
    elif mode == 'users':
        Graph = __build_graph_from_users(questions)
    else:
        raise Exception()
    if save:
        suffix = f'stack_{mode}'
        output_path = Path(utils.INTERIM_DATA_PATH, utils.get_graph_name(suffix=suffix)) if path is None else path
        utils.pickle_graph(Graph, output_path)
    return Graph

def __build_graph_from_comments(questions):
    Graph = nx.DiGraph()
    if len(questions['items']) > 1:
        n = randint(0, len(questions['items'])-1)
        questions = questions['items'][n:n+1]
    else:
        questions = questions['items']
    for question in questions:
        question_id = stack_utils.get_question_id(question)
        question_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(question))
        Graph.add_node(question_id)
        answers = get_answers(question_id, site=site)

        for answer in answers['items']:
            answer_id = stack_utils.get_answer_id(answer)
            answer_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(answer))
            similarity = TextAnalyzer.get_similarity(stack_utils.get_text(question), stack_utils.get_text(answer))
            # compute the weight of the edge
            weight = get_edge_weight(similarity, answer_sentiment, question_sentiment)
            Graph.add_edge(answer_id, question_id, weight=weight)
            comments = get_comments(answer_id, site=site)

            for comment in comments['items']:
                comment_id = stack_utils.get_comment_id(comment)
                comment_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(comment))
                similarity = TextAnalyzer.get_similarity(stack_utils.get_text(answer), stack_utils.get_text(comment))
                # compute the weight of the edge
                weight = get_edge_weight(similarity, comment_sentiment, answer_sentiment)
                Graph.add_edge(comment_id, answer_id, weight=weight)
    return Graph

def __build_graph_from_users(questions):
    Graph = nx.DiGraph()
    if len(questions['items']) > 1:
        n = randint(0, len(questions['items'])-1)
        questions = questions['items'][n:n+1]
    else:
        questions = questions['items']
    for question in questions:
        question_id        = stack_utils.get_question_id(question)
        question_user_id   = stack_utils.get_user_id(question)
        question_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(question))
        Graph.add_node(question_user_id)
        answers = get_answers(question_id, site=site)

        for answer in answers['items']:
            answer_id        = stack_utils.get_answer_id(answer)
            answer_user_id   = stack_utils.get_user_id(answer)
            answer_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(answer))
            similarity       = TextAnalyzer.get_similarity(stack_utils.get_text(answer), stack_utils.get_text(question))
            # compute the weight of the edge
            weight = get_edge_weight(similarity, answer_sentiment, question_sentiment)
            Graph.add_edge(answer_user_id, question_user_id, weight=weight)
            comments = get_comments(answer_id, site=site)

            for comment in comments['items']:
                comment_id        = stack_utils.get_comment_id(comment)
                comment_user_id   = stack_utils.get_user_id(comment)
                comment_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(comment))
                similarity        = TextAnalyzer.get_similarity(stack_utils.get_text(comment), stack_utils.get_text(answer))
                # compute the weight of the edge
                weight = get_edge_weight(similarity, comment_sentiment, answer_sentiment)
                Graph.add_edge(comment_user_id, answer_user_id, weight=weight)
    return Graph

def get_questions(questions, num_questions=1, site=None, order='desc', sort='votes', filter='withbody'):
    site.page_size = num_questions
    return site.fetch(questions, order=order, sort=sort, filter=filter)

def get_answers(question_id, site=None, order='desc', sort='votes', filter='withbody'):
    return site.fetch(stack_utils.ANSWERS_TO % question_id, order=order, sort=sort, filter=filter)

def get_comments(answer_id, site=None, order='desc', sort='votes', filter='withbody'):
    return site.fetch(stack_utils.COMMENTS_TO % answer_id, order='desc', sort='votes', filter='withbody')
