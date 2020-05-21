% :- module(baf, [baf/1]).

% [af_semantics].

% import af graph
% :- ["../../data/prolog/kb/example/af"].

% param Argument - INPUT   - argument
% param Defender - OUTPUT  - a defender
defender(Argument, Defender) :-
	attack(Attacker, Argument),
	attack(Defender, Attacker).

% param Argument - INPUT   - argument
% param Defender - OUTPUT  - defenders set
defenders(Argument, DefendersSet) :-
	setof(Defender, defender(Argument, Defender), DefendersSet).

%		setof(SE, stable_extension(SE), SEs).
defends(Set, Argument) :-
	forall(attack(Attacker, Argument), (attack(Defender, Attacker) , member(Defender, Set))).

% va ma non genera, con variabile va out-of-global-stack
% con set fissato dice true.
conflict_free_set2(Set) :-
	forall(pair(A, B, Set), \+ attack(A, B)).

% defends(Set, Argument) :-
% 		forall(attack(Attacker, Argument), (attack(Defender, Attacker) , member(Defender, Set))).

% TODO change conflict free definition it's ugly
% param Set - OUTPUT  - a conflict free set
conflict_free_set([]).
conflict_free_set([A]) :-
	argument(A).
conflict_free_set(CF) :-
  setof(A1, argument(A1), Args),
	sub_set(CF, Args),
	setof([A4, A5], pair(A4, A5, CF), Pairs),
	setof([A6, A7], attack(A6, A7), Attacks),
	intersection(Pairs, Attacks, []).

conflict_free_sets(CFs) :-
	findall(CF, conflict_free_set(CF), CFs).

% trovando un sottoinsieme conflict free, i suoi nodi potrebbero essere molto
% sparsi in un grafo grande, (cosa risolta nelle estensioni successive),
% rankare gli insiemi conflict free in base ad una misura di distanza dei suoi nodi interni

% misura di distanza fra nodi (Node, Node) -> double
% misura di vicinanza tra i nodi di un insieme (Set) -> double
% rank in base alla misura di vicinanza


stable_extension(SE) :-
  setof(A1, argument(A1), Args),
	conflict_free_set(SE),
	subtract(Args, SE, Others),
	forall(member(X,Others), set_attack(SE,X)).

stable_extensions(SEs) :-
	setof(SE, stable_extension(SE), SEs).



admissible_extension(AE) :-
  % setof(A1, argument(A1), Args),
	conflict_free_set(AE),
	forall(member(X,AE), set_defense(AE,X) ; not_attacked(X)).

admissible_extensions(AEs) :-
	setof(AE, admissible_extension(AE), AEs).



preferred_extension(PE) :-
	admissible_extension(PE),
	forall(admissible_extension(PE1), \+ strictly_contained(PE, PE1)).

preferred_extensions(AEs) :-
	setof(AE, preferred_extension(AE), AEs).



% complete_extension(CE) :-
% 	admissible_extension(CE),
% 	forall(set_defense(CE, Argument), member(Argument, CE) ; not_attacked(Argument)).

complete_extension(CE) :-
	admissible_extension(CE),
	forall(defends(CE, Element), member(Element, CE)).

complete_extensions(CEs) :-
	setof(CE, complete_extension(CE), CEs).

% ==============================================================================
% ============================== OTHERS ========================================
% ==============================================================================

set_attack(Set, Argument) :-
	member(Attacker, Set),
	attack(Attacker, Argument),
	!.

set_defense(Set, Argument) :-
	defender(Argument, Defender),
	member(Defender, Set). %, !.


% if qualcuno fuori dal Set attacca Argument allora uno dentro al Set attacca l'attaccante



not_attacked(Argument) :-
	\+ attack(_, Argument).

attackers_of_a_set(Set, Attackers) :-
	setof(AE, admissible_extension(AE), AEs).

% ==============================================================================
% ============================= HELPERS ========================================
% ==============================================================================

pair(A, B, Set) :-
	member(A, Set),
	member(B, Set),
	A \= B.

sub_set([], []) :- !.
sub_set([E|Tail], [E|NTail]) :-
	sub_set(Tail, NTail).
sub_set(Tail, [_|NTail]) :-
	sub_set(Tail, NTail).



strictly_contained(Set1, Set2) :-
	sub_set(Set1, Set2),
	Set1 \= Set2.
