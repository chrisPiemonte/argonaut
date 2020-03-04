import sys, os
import networkx as nx
from random import randint
from stackapi import StackAPI
import crawler.utils.io as io
import crawler.utils.common_utils as utils
from crawler.argumentation.mine.common import *
import crawler.utils.stack_utils as stack_utils
import crawler.text.TextAnalyzer as TextAnalyzer
from crawler.argumentation.convert import common
from crawler.argumentation.convert import to_prolog

site = StackAPI('stackoverflow')

def get_debate_graph(question=None, mode='comments', save=True, path=None,
                     multiedges=False, framework=common.BWAF, n_decimal=2, verbose=False):
    questions_request = stack_utils.QUESTION_URL % question if question is not None else stack_utils.QUESTIONS_URL
    questions = get_questions(questions=questions_request, site=site)
    Graph = None
    if mode == 'comments':
        Graph = __build_graph_from_comments(questions)
    elif mode == 'users':
        Graph = __build_graph_from_users(questions)
        if not multiedges:
            Graph = merge_multiedges(Graph)
    else:
        raise Exception()
    remove_nones(Graph)
    if save:
        suffix = f'stack_{mode}'
        io.save_graph(Graph, suffix, path=path, mode=mode, framework=framework, n_decimal=n_decimal, verbose=verbose)
    if verbose:
        print(f'NUMBER OF NODES IN THE GRAPH:      {count_nodes(Graph)}')
        print(f'NUMBER OF EDGES IN THE GRAPH:      {count_edges(Graph)}')
        print(f'NUMBER OF NULL EDGES IN THE GRAPH: {count_edges_with_zero_weight(Graph, n_decimal=n_decimal)}', '\n')
    return Graph

def __build_graph_from_comments(questions):
    Graph = nx.MultiDiGraph()
    if len(questions['items']) > 1:
        n = randint(0, len(questions['items'])-1)
        questions = questions['items'][n:n+1]
    else:
        questions = questions['items']
    for question in questions:
        question_id = stack_utils.get_question_id(question)
        question_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(question))
        Graph.add_node(question_id, text=stack_utils.get_text(question), user=stack_utils.get_user_id(question))
        answers = get_answers(question_id, site=site)

        for answer in answers['items']:
            answer_id = stack_utils.get_answer_id(answer)
            answer_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(answer))
            similarity = TextAnalyzer.get_similarity(stack_utils.get_text(question), stack_utils.get_text(answer))
            # compute the weight of the edge
            weight = get_edge_weight(similarity, answer_sentiment, question_sentiment)

            Graph.add_node(answer_id, text=stack_utils.get_text(answer), user=stack_utils.get_user_id(answer))
            Graph.add_edge(answer_id, question_id, weight=weight)
            comments = get_comments(answer_id, site=site)

            for comment in comments['items']:
                comment_id = stack_utils.get_comment_id(comment)
                comment_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(comment))
                similarity = TextAnalyzer.get_similarity(stack_utils.get_text(answer), stack_utils.get_text(comment))
                # compute the weight of the edge
                weight = get_edge_weight(similarity, comment_sentiment, answer_sentiment)

                Graph.add_node(comment_id, text=stack_utils.get_text(comment), user=stack_utils.get_user_id(comment))
                Graph.add_edge(comment_id, answer_id, weight=weight)
    return Graph

def __build_graph_from_users(questions):
    Graph = nx.MultiDiGraph()
    if len(questions['items']) > 1:
        n = randint(0, len(questions['items'])-1)
        questions = questions['items'][n:n+1]
    else:
        questions = questions['items']
    for question in questions:
        question_id        = stack_utils.get_question_id(question)
        question_user_id   = stack_utils.get_user_id(question)
        question_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(question))

        if question_user_id in Graph.node:
            Graph.node[question_user_id]['text'].add(stack_utils.get_text(question))
        else:
            Graph.add_node(question_user_id, text={stack_utils.get_text(question)})
        # Graph.add_node(question_user_id)
        answers = get_answers(question_id, site=site)

        for answer in answers['items']:
            answer_id        = stack_utils.get_answer_id(answer)
            answer_user_id   = stack_utils.get_user_id(answer)
            answer_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(answer))
            similarity       = TextAnalyzer.get_similarity(stack_utils.get_text(answer), stack_utils.get_text(question))
            # compute the weight of the edge
            weight = get_edge_weight(similarity, answer_sentiment, question_sentiment)

            if answer_user_id in Graph.node:
                Graph.node[answer_user_id]['text'].add(stack_utils.get_text(answer))
            else:
                Graph.add_node(answer_user_id, text={stack_utils.get_text(answer)})
            Graph.add_edge(answer_user_id, question_user_id, weight=weight)
            comments = get_comments(answer_id, site=site)

            for comment in comments['items']:
                comment_id        = stack_utils.get_comment_id(comment)
                comment_user_id   = stack_utils.get_user_id(comment)
                comment_sentiment = TextAnalyzer.get_sentiment(stack_utils.get_text(comment))
                similarity        = TextAnalyzer.get_similarity(stack_utils.get_text(comment), stack_utils.get_text(answer))
                # compute the weight of the edge
                weight = get_edge_weight(similarity, comment_sentiment, answer_sentiment)

                if comment_user_id in Graph.node:
                    Graph.node[comment_user_id]['text'].add(stack_utils.get_text(comment))
                else:
                    Graph.add_node(comment_user_id, text={stack_utils.get_text(comment)})
                Graph.add_edge(comment_user_id, answer_user_id, weight=weight)
    return Graph

def get_questions(questions, num_questions=1, site=None, order='desc', sort='votes', filter='withbody'):
    site.page_size = num_questions
    return site.fetch(questions, order=order, sort=sort, filter=filter)

def get_answers(question_id, site=None, order='desc', sort='votes', filter='withbody'):
    return site.fetch(stack_utils.ANSWERS_TO % question_id, order=order, sort=sort, filter=filter)

def get_comments(answer_id, site=None, order='desc', sort='votes', filter='withbody'):
    return site.fetch(stack_utils.COMMENTS_TO % answer_id, order='desc', sort='votes', filter='withbody')
