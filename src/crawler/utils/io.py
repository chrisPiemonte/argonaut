import numpy as np
from pathlib import Path
import argonaut.utils.common_utils as utils
from argonaut.argumentation.convert import common
from argonaut.argumentation.convert import to_lines
from argonaut.argumentation.convert import to_prolog
import tweepy, gensim, nltk, yaml, os, sys, pickle, datetime, csv


HEADER_COMMENTS_NODES = ['COMMENT_ID','TEXT','USER']
HEADER_USERS_NODES    = ['USER_ID','TEXT']

HEADER_EDGES = ['SOURCE','DEST','WEIGHT']

def write_nodes(Graph, file_path, mode='comments', sep=','):
    file_path = Path(file_path)
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with open(str(file_path), mode='w') as file:
        if mode == 'comments':
            # WRITE HEADER
            writer = csv.writer(file, delimiter=sep)
            writer.writerow([col for col in HEADER_COMMENTS_NODES])
            for line in to_lines.comment_nodes_to_lines(Graph, sep=sep):
                writer.writerow([elem for elem in line])
        elif mode == 'users':
            writer = csv.writer(file, delimiter=sep)
            writer.writerow([col for col in HEADER_USERS_NODES])
            for line in to_lines.user_nodes_to_lines(Graph, sep=sep):
                writer.writerow([elem for elem in line])
        else:
            raise(Exception)
    print(f'CSV nodes writed successfully at: {file_path}.')

def write_edges(Graph, file_path, sep=',', n_decimal=2):
    file_path = Path(file_path)
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with open(str(file_path), mode='w') as file:
        writer = csv.writer(file, delimiter=sep)
        writer.writerow([col for col in HEADER_EDGES])
        for line in to_lines.edges_to_lines(Graph, sep=sep, n_decimal=n_decimal):
            writer.writerow([elem for elem in line])
    print(f'CSV edges writed successfully at: {file_path}.')

def save_graph_description_csv(Graph, path, mode='comments', sep=',', n_decimal=2):
    nodes_path = str(path).replace('_description.csv', '_nodes_description.csv')
    edges_path = str(path).replace('_description.csv', '_edges_description.csv')

    write_nodes(Graph, nodes_path, mode=mode, sep=sep)
    write_edges(Graph, edges_path, sep=sep, n_decimal=n_decimal)


########################################################


def pickle_graph(Graph, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(str(path), 'wb') as file:
        pickle.dump(Graph, file)
    print(f'Graph pickled successfully at: {path}.')

def save_graph(Graph, suffix, path=None, framework=common.BWAF, mode='comments', n_decimal=2, verbose=False):

    facts = to_prolog.to_facts(Graph, framework=framework, n_decimal=n_decimal, verbose=verbose)

    graph_name    = get_graph_name(suffix=suffix)
    facts_name    = get_facts_name(graph_name=graph_name, framework=framework)
    csv_desc_name = get_graph_csv_description_name(graph_name=graph_name, framework=framework)
    plot_name     = get_graph_plot_name(graph_name=graph_name, framework=framework)

    # graph_output_path = Path(utils.INTERIM_DATA_PATH, graph_name) if path is None else path + '_graph.pickle'
    # facts_output_path = Path(utils.PROLOG_DATA_PATH, facts_name) if path is None else path + '_facts.pl'

    graph_output_path    = Path(utils.OUTPUT_DATA_PATH, graph_name) if path is None else path + '_graph.pickle'
    facts_output_path    = Path(utils.OUTPUT_DATA_PATH, facts_name) if path is None else path + '_facts.pl'
    csv_desc_output_path = Path(utils.OUTPUT_DATA_PATH, csv_desc_name) if path is None else path + '_description.csv'
    plot_output_path     = Path(utils.OUTPUT_DATA_PATH, plot_name) if path is None else path + '_plot.png'

    # SAVE
    pickle_graph(Graph, graph_output_path)
    save_facts(facts, facts_output_path)
    save_graph_description_csv(Graph, csv_desc_output_path, mode=mode, sep=',', n_decimal=n_decimal)

    print('Everything saved successfully.', '\n')


def load_pickled_graph(path):
    with open(str(path), 'rb') as file:
        Graph = pickle.load(file)
    return Graph

def get_graph_name(suffix=''):
    return f'{__get_time()}_{suffix}_graph.pickle'

def __get_time(format='%y%m%d-%H%M%S'):
    return datetime.datetime.now().strftime(format)

def save_facts(facts, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(str(path), 'w') as file:
        for fact in sorted(facts):
            file.write(str(fact) + '\n')
    print(f'Prolog facts saved successfully at: {path}.')

def get_facts_name(suffix='', graph_name=None, framework=''):
    facts_name = f'{__get_time()}_{suffix}_{framework}_facts.pl'
    if graph_name is not None:
        graph_path = Path(graph_name)
        facts_name = graph_path.name.replace('graph.pickle', f'{framework}_facts.pl')
    return facts_name

def get_graph_csv_description_name(suffix='', graph_name=None, framework=''):
    csv_description_name = f'{__get_time()}_{suffix}_{framework}_description.csv'
    if graph_name is not None:
        graph_path = Path(graph_name)
        csv_description_name = graph_path.name.replace('graph.pickle', f'{framework}_description.csv')
    return csv_description_name

def get_graph_plot_name(suffix='', graph_name=None, framework=''):
    plot_name = f'{__get_time()}_{suffix}_{framework}_plot.png'
    if graph_name is not None:
        graph_path = Path(graph_name)
        plot_name = graph_path.name.replace('graph.pickle', f'{framework}_plot.png')
    return plot_name









def save_graph_plot(Graph, path):

    print(f'Graph plot saved successfully at: {path}.', '\n')
    pass
