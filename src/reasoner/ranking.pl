:- module(ranking, [
    rank/3,
    score_by_paths/2,
    score_by_paths_normalized/2,
    score_by_denfence_indegree/2
]).


:- ensure_loaded(utils).
:- ensure_loaded(graph).
% :- use_module(library(lists)).
:- use_module(library(pairs)).
:- style_check(-singleton).

% :- ["../../data/prolog/kb/example/baf"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SCORE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% usage trace([a, f, g], S). -> 10
score_by_paths([], 0) :- !.
score_by_paths([H], 0) :- !.
score_by_paths([Node1, Node2 | Nodes], Score) :-
    sum_distances(Node1, [Node2 | Nodes], FirstSum),
    sum_distances(Node2, Nodes, RestSum),
    !,
    Score is FirstSum + RestSum.

% usage score_by_paths([a, f, g], S). -> 10
score_by_paths_normalized([], 0) :- !.
score_by_paths_normalized([H], 0) :- !.
score_by_paths_normalized([Node1, Node2 | Nodes], Score) :-
    sum_distances(Node1, [Node2 | Nodes], FirstSum),
    sum_distances(Node2, Nodes, RestSum),
    !,
    length([Node1, Node2 | Nodes], L),
    Score is (FirstSum + RestSum) / L.


sum_distances(Node, [], 0).
sum_distances(Node, [OtherNode | Nodes], Sum) :-
    (graph:shortest_path(graph:edge, Path, Node, OtherNode) ->
        graph:shortest_path(graph:edge, Path, Node, OtherNode),
        length(Path, CurrLength)
    ;
        setof(Arg, argument(Arg), Args),
        length(Args, CurrLength)
    ),
    
    sum_distances(Node, Nodes, RestSum), 
    !,
    Sum is CurrLength + RestSum.


score_by_attack_indegree([], 0) :- !.
score_by_attack_indegree([H|T], Score) :- 
    graph:set_defense_relationships,
    graph:indegree(attack, H, InDegreeH),
    score_by_denfence_indegree(T, Rest),
    Score is InDegreeH + Rest.


% score_by_indegree([], EdgeType, 0) :- !.
% score_by_indegree([H|T], EdgeType, Score) :- 
%     graph:indegree(EdgeType, H, InDegreeH),
%     score_by_indegree(T, EdgeType, Rest),
%     Score is InDegreeH + Rest.

score_by_denfence_indegree([], 0) :- !.
score_by_denfence_indegree([H|T], Score) :- 
    graph:set_defense_relationships,
    graph:indegree(graph:defends, H, InDegreeH),
    score_by_denfence_indegree(T, Rest),
    Score is (-1 * InDegreeH) + Rest.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RANK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% param 1 - INPUT  - ScoringFunc/2 is a function that, given an extension, returns a score
% param 2 - INPUT  - List of lists - represent an extension
% param 3 - OUTPUT - List of lists - ranked by ScoringFunc/2
rank(ScoringFunc, Extension, RankedExtension) :-
    map_list_to_pairs(ScoringFunc, Extension, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, RankedExtension).


