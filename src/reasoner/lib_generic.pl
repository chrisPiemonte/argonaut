:- module(lib_generic, [read_int_choice/4]).

:- ensure_loaded(library(system)).
	% file_exists/1

:- ensure_loaded(library(lists)).
	% append/3


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

