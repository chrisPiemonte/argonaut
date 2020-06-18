
:- module(graph, [
    path/4,
    paths/4,
    shortest_path/4,
    defender/2,
    defenders/2,
    indegree/3,
    closeness/3,
    farness/3
]).



:- ensure_loaded(utils).
:- ensure_loaded("arguer/lib_arguer").
:- ensure_loaded(library(lists)).

edge(U, V) :-
    attack(U, V) ; attack(V, U).


diredge(U, V) :-
    attack(U, V).

%
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
    utils:select_element(utils:get_shorter_list, Paths, Path),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NODE METRICS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEFENSE GRAPH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% param Argument - INPUT   - argument
% param Defender - OUTPUT  - a defender
defender(Argument, Defender) :-
    attack(Attacker, Argument),
    attack(Defender, Attacker).

% param Argument - INPUT   - argument
% param Defender - OUTPUT  - defenders set
defenders(Argument, DefendersSet) :-
	setof(Defender, defender(Argument, Defender), DefendersSet).

set_defense_relationships :-
    (\+current_predicate(graph:defends/2) ->
        (current_predicate(support/2) ->
            forall(support(X, Y), assertz(graph:defends(X, Y)))
        ;
            true
        ),
        forall(argument(A),
            (defenders(A, Defenders) ->
                forall(member(D, Defenders), assertz(graph:defends(D, A)))
            ;
                true
            )
        )
    ;
        true
    ).




% ACTUAL METRICS based on centrality, modularity or something else

indegree(EdgeType, Node, Val) :-
    findall(Source, call(EdgeType, Source, Node), Sources),
    length(Sources, Val).


closeness(EdgeType, Node, Closeness) :-
    setof(Arg, argument(Arg), Args),
    length(Args, N),
    avg_all_shortest_paths_rec(EdgeType, Node, Args, 0, Val),
    Closeness is (N - 1) / Val.

farness(EdgeType, Node, Farness) :-
    setof(Arg, argument(Arg), Args),
    length(Args, N),
    avg_all_shortest_paths_rec(EdgeType, Node, Args, 0, Val),
    Farness is Val / (N - 1).

avg_all_shortest_paths_rec(_, _, [], Acc, Acc) :- !.
avg_all_shortest_paths_rec(EdgeType, Node, [OtherNode|Rest], Acc, Res) :-
    (shortest_path(EdgeType, ShortestPath, Node, OtherNode) ->
        length(ShortestPath, CurrVal)
    ;
        % TODO: Choose how to behave
        setof(Arg, argument(Arg), Args),
        length(Args, CurrVal)
    ),
    % write(Node), write(OtherNode), write(' -> '), write(CurrVal), nl, nl,
    NextAcc is CurrVal + Acc,
    avg_all_shortest_paths_rec(EdgeType, Node, Rest, NextAcc, Res).
