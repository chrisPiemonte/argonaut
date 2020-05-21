:- ensure_loaded(lib_generic).
:- [af_semantics].
:- [ranking].
:- [utils].

argonaut :-
	repeat,
		argonaut_heading,
		argonaut_main_menu(MenuChoice),
		argonaut_process_choice(MenuChoice),
		MenuChoice == 0,
	!.

sub_argonaut :-
	repeat,
		argonaut_main_submenu(MenuChoice),
		argonaut_process_subchoice(MenuChoice),
		MenuChoice == 0,
	!.

subsub_argonaut :-
	repeat,
		argonaut_main_subsubmenu(MenuChoice),
		argonaut_process_subsubchoice(MenuChoice),
		MenuChoice == 0,
	!.


argonaut_heading :-
    nl,
    write('\e[H\e[2J'),
 	write('                                         _   '),nl,
	write('  __ _ _ __ __ _  ___  _ __   __ _ _   _| |_ '),nl,
	write(' / _` | `__/ _` |/ _ \\| `_ \\ / _` | | | | __|'),nl,
	write('| (_| | | | (_| | (_) | | | | (_| | |_| | |_ '),nl,
    write(' \\__,_|_|  \\__, |\\___/|_| |_|\\__,_|\\__,_|\\__|'),nl,
    write('           |___/                             '),nl,nl,
	write('Ranking AF extensions.'),nl,
	write('Artificial Intelligence - Christopher Piemonte'),nl,
	nl.

argonaut_main_menu(MenuChoice) :-
	write('       1 - Load mined data (AF)'),nl,
	write('       2 - Choose an extension semantic and rank its elements by a criteria'),nl,
	write('       0 - Quit'),nl,nl,
	read_int_choice('Enter your choice', 0, 2, MenuChoice),
	!.

argonaut_process_choice(0) :- % Exit
	nl,write('Bye'),nl.

argonaut_process_choice(1) :- % AF
    nl,read(InputPath),
    write(InputPath),
    consult(InputPath),
    nl.
argonaut_process_choice(2) :- % VAF
	!,
	sub_argonaut.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

argonaut_main_submenu(MenuChoice) :-
	write('- Choose an extension semantic: '),nl,
	write('       1 - Conflict free'),nl,
	write('       2 - Preferred'),nl,
	write('       0 - Quit'),nl,nl,
	read_int_choice('Enter your choice', 0, 2, MenuChoice),
	!.

argonaut_process_subchoice(0) :- % Exit
	nl,write('Bye'),nl.

argonaut_process_subchoice(1) :- % AF
    !,
    nl,
    write('Conflict free sets gen:'),nl,
    conflict_free_sets(CFs),
    % write(CFs),nl,
    nb_setval(extension, CFs),
    subsub_argonaut,
    nl.
argonaut_process_subchoice(2) :- % VAF
    !,
    nl,
    write('Preferred sets gen:'),nl,
    preferred_extensions(AEs),nl,
    % write(AEs),
    nb_setval(extension, AEs),
    subsub_argonaut,
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

argonaut_main_subsubmenu(MenuChoice) :-
	write('- Rank its elements by: '),nl,
	write('       1 - Distance'),nl,
	write('       2 - Normalized distance'),nl,
	write('       0 - Quit'),nl,nl,
	read_int_choice('Enter your choice', 0, 2, MenuChoice),
	!.

argonaut_process_subsubchoice(0) :- % Exit
	nl,write('Bye'),nl,nl,nl.

argonaut_process_subsubchoice(1) :- % AF
    !,
    nl,
    nb_getval(extension, Extension),
    write('Original extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by distance: '),nl,
    rank(score_by_paths, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.

argonaut_process_subsubchoice(2) :- % AF
    !,
    nl,
    nb_getval(extension, Extension),
    write('Original extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by normalized distance: '),nl,
    rank(score_by_paths_normalized, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.