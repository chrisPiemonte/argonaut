:- module(utils, [
    read_int_choice/4,
    get_shorter_list/3,
    select_element/3
]).

:- ensure_loaded(library(system)).
	% file_exists/1

:- ensure_loaded(library(lists)).
	% append/3


% param 1 - INPUT  - 1st list
% param 2 - INPUT  - 2nd list
% param 3 - OUTPUT - shorter list
get_shorter_list(List1, List2, ShorterList) :- 
    length(List1, L1),
    length(List2, L2),
    (  L1 =< L2 -> 
        ShorterList = List1
    ;  
        ShorterList = List2
    ).


% general predicate that scans a list to find a single member defined by a given goal
select_element(Goal, [Head | Tail], Selected) :-
    select_element(Goal, Tail, Head, Selected).

select_element(_Goal, [], Selected, Selected).

select_element(Goal, [Head | Tail], Current, FinalSelected) :-
    call(Goal, Head, Current, Selected),
    select_element(Goal, Tail, Selected, FinalSelected).



/* read_int_choice(D,V1,V2,R)
   Accetta dall'input corrente un intero compreso fra i due valori V1 e V2
*/
read_int_choice(Question,ValMin,ValMax,Answer):-
	write(Question),
	repeat,
		write(' ('),write(ValMin),write('-'),write(ValMax),write('): '),
		read(Answer),
		integer(Answer),
		Answer >= ValMin,
		Answer =< ValMax,
	!.
