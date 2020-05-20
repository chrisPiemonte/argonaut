
:- [af_semantics].
:- [utils].
% :- use_module(library(lists)).
:- use_module(library(pairs)).
:- ["../../data/prolog/kb/example/dummy_graph"].

% gino(SE) :-
% stable_extensions(SE).

% graph distance metrics

% PROVARE CON 
% ?- preferred_extensions(AEs).
% AEs = [[a, c, f], [a, c, g, h], [a, d, f], [a, d, g, h]].

% devo caricare il risultato di una extension ed il grafo totale anche
% così posso rankare ogni subset della extension



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


path(R_2, Xs, A, Z) :- 
    all_dif(Xs),
    walk(R_2, Xs, A, Z).

all_dif(Xs) :-                          % enforce pairwise term inequality
   freeze(Xs, all_dif_aux(Xs,[])).      % (may be delayed)

all_dif_aux([], _).
all_dif_aux([E|Es], Vs) :-               
   maplist(dif(E), Vs),                 % is never delayed
   freeze(Es, all_dif_aux(Es,[E|Vs])).  % (may be delayed)

:- meta_predicate walk(2, ?, ?, ?).
walk(R_2, [X0|Xs], X0,X) :-
   walk_from_to_step(Xs, X0,X, R_2).

:- meta_predicate walk_from_to_step(?, ?, ?, 2).
walk_from_to_step([], X,X, _).
walk_from_to_step([X1|Xs], X0,X, R_2) :-
   call(R_2, X0,X1),
   walk_from_to_step(Xs, X1,X, R_2).


% usage
% param 1 - INPUT  - predicate
% param 2 - OUTPUT - all paths
% param 2 - INPUT  - starting node of the resulting path
% param 2 - INPUT  - ending node of the resulting path
paths(R_2, Paths, A, Z) :-
	findall(Path, path(R_2, Path, A, Z), Paths).


% get the first shortest path it finds
shortest_path(R_2, Path, A, Z) :-
    paths(R_2, Paths, A, Z),
    select_element(get_shorter_list, Paths, Path),
    !.

% usage score_by_paths([a, f, g], S). -> 10
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
    shortest_path(edge, Path, Node, OtherNode),
    length(Path, CurrLength),
    sum_distances(Node, Nodes, RestSum), 
    !,
    Sum is CurrLength + RestSum.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% param 1 - INPUT  - ScoringFunc/2 is a function that, given an extension, returns a score
% param 2 - INPUT  - List of lists - represent an extension
% param 3 - OUTPUT - List of lists - ranked by ScoringFunc/2
rank(ScoringFunc, Extension, RankedExtension) :-
    map_list_to_pairs(ScoringFunc, Extension, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, RankedExtension).


