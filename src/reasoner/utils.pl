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
