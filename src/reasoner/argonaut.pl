%:- ensure_loaded(lib_generic).
:- ensure_loaded("arguer/semantics").
% :- [af_semantics].
:- ensure_loaded("../pazi/arguer/example.af").
:- [ranking].
:- [utils].
:- style_check(-singleton).
:- set_prolog_flag(unknown, fail).
:- ensure_loaded(library(lists)).

argonaut :-
	repeat,
		argonaut_heading,
		argonaut_main_menu(MenuChoice),
		argonaut_process_choice(MenuChoice),
		MenuChoice == 0,
	!.

extension_menu :-
	repeat,
		extension_menu_options(MenuChoice),
		extension_menu_process_choice(MenuChoice),
		MenuChoice == 0,
	!.

rank_menu :-
	repeat,
		rank_menu_options(MenuChoice),
		rank_menu_process_choice(MenuChoice),
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
	extension_menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extension_menu_options(MenuChoice) :-
	write('- Choose an extension semantic: '),nl,
	write('       1 - Conflict free'),nl,
	write('       2 - Stable'),nl,
	write('       3 - Admissible'),nl,
	write('       4 - Complete'),nl,
	write('       5 - Preferred'),nl,
	write('       6 - Grounded'),nl,
	write('       0 - Quit'),nl,nl,
	read_int_choice('Enter your choice', 0, 6, MenuChoice),
	!.

extension_menu_process_choice(0) :- % Exit
	nl,write('Bye'),nl.

extension_menu_process_choice(1) :- % AF
    !,
    nl,
    get_conflict_free_sets_wrapper(CFSets),
    nb_setval(extension, CFSets),
    nb_setval(extension_desc, 'Conflict-free '),
    rank_menu,
    nl.
extension_menu_process_choice(2) :- 
    !,
    nl,
    get_stable_extensions_wrapper(StableExtensions),
    nb_setval(extension, StableExtensions),
    nb_setval(extension_desc, 'Stable '),
    rank_menu,
    nl.
extension_menu_process_choice(3) :- 
    !,
    nl,
    get_admissible_sets_wrapper(AdmSets),
    nb_setval(extension, AdmSets),
    nb_setval(extension_desc, 'Admissible '),
    rank_menu,
    nl.

extension_menu_process_choice(4) :- 
    !,
    nl,
    get_complete_extensions_wrapper(CompleteExtensions),
    nb_setval(extension, CompleteExtensions),
    nb_setval(extension_desc, 'Complete '),
    rank_menu,
    nl.

extension_menu_process_choice(5) :- 
    !,
    nl,
    get_preferred_extensions_wrapper(PreferredExtensions),
    nb_setval(extension, PreferredExtensions),
    nb_setval(extension_desc, 'Preferred '),
    rank_menu,
    nl.

extension_menu_process_choice(6) :- 
    !,
    nl,
    get_grounded_extensions_wrapper(GroundedExtensions),
    nb_setval(extension, GroundedExtensions),
    nb_setval(extension_desc, 'Grounded '),
    rank_menu,
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rank_menu_options(MenuChoice) :-
	write('- Rank its elements by: '),nl,
	write('              1 - Distance'),nl,
	write('              2 - Normalized distance'),nl,
	write('              3 - In-degree in attack-graph'),nl,
	write('              4 - In-degree in defence-graph'),nl,
	write('              0 - Quit'),nl,nl,
	read_int_choice('Enter your choice', 0, 4, MenuChoice),
	!.

rank_menu_process_choice(0) :- % Exit
	nl,write('Bye'),nl,nl,nl.

rank_menu_process_choice(1) :- 
    !,
    nl,
    nb_getval(extension, Extension),
    nb_getval(extension_desc, ExtensionDesc),
    write(ExtensionDesc), write('Extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by distance: '),nl,
    rank(ranking:score_by_paths, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.

rank_menu_process_choice(2) :- 
    !,
    nl,
    nb_getval(extension, Extension),
    nb_getval(extension_desc, ExtensionDesc),
    write(ExtensionDesc), write('Extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by normalized distance: '),nl,
    rank(ranking:score_by_paths_normalized, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.

rank_menu_process_choice(3) :- 
    !,
    nl,
    nb_getval(extension, Extension),
    nb_getval(extension_desc, ExtensionDesc),
    write(ExtensionDesc), write('Extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by in-degree in attack graph: '),nl,
    rank(ranking:score_by_attack_indegree, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.

rank_menu_process_choice(4) :- 
    !,
    nl,
    nb_getval(extension, Extension),
    nb_getval(extension_desc, ExtensionDesc),
    write(ExtensionDesc), write('Extension: '),nl,
    write(Extension),nl,nl,
    write('Ranked by in-degree in defence graph: '),nl,
    rank(ranking:score_by_denfence_indegree, Extension, RankedExtension),
    write(RankedExtension),nl,
    nl.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WRAPPERS %%%%%%%%%%%%%%%%%%%%%%%%%

get_conflict_free_sets_wrapper(CFSets) :-
    (\+current_predicate(cf:cfree/1) ->
        semantics:get_abstract_arguments(Arguments),
        semantics:set_conflict_free_sets(Arguments)
    ;
        true
    ),
    semantics:get_conflict_free_sets(CFSets).

get_admissible_sets_wrapper(AdmissibleSets) :-
    get_conflict_free_sets_wrapper(CFSets),
    (\+current_predicate(extensions:admissible/1) ->
        semantics:get_abstract_arguments(Arguments),
        semantics:set_acceptable_arguments(Arguments, CFSets)
    ;
        true
    ),
    semantics:get_admissible_sets(AdmissibleSets).

get_complete_extensions_wrapper(CompleteExtensions) :-
    get_admissible_sets_wrapper(AdmissibleSets),
    (\+current_predicate(extensions:complete/1) ->
        semantics:set_completeness(AdmissibleSets)
    ;
        true
    ),
	semantics:get_complete_extensions(CompleteExtensions).

get_preferred_extensions_wrapper(PreferredExtensions) :-
    get_complete_extensions_wrapper(CompleteExtensions),
    (\+current_predicate(extensions:preferred/1) ->
        semantics:is_preferred(CompleteExtensions)
    ;
        true
    ),
	semantics:get_preferred_extensions(PreferredExtensions).

get_grounded_extensions_wrapper(GroundedExtensions) :-
    get_complete_extensions_wrapper(CompleteExtensions),
    (\+current_predicate(extensions:grounded/1) ->
        semantics:is_grounded(CompleteExtensions)
    ;
        true
    ),
	semantics:get_grounded_extension(GroundedExtensions).

get_stable_extensions_wrapper(StableExtensions) :-
    get_preferred_extensions_wrapper(PreferredExtensions),
    (\+current_predicate(extensions:stable/1) ->
        semantics:get_abstract_arguments(Arguments),
        forall(member(PrefExt, PreferredExtensions),
            (semantics:is_stable(PrefExt, Arguments) ->
                assertz(extensions:stable(PrefExt))
            ;
                true
            )
        )
    ;
        true
    ),
    semantics:get_stable_extensions(StableExtensions).