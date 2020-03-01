% :- module(baf, [baf/1]).

% import baf graph
:- ['../data/baf'].


menu(MenuChoice) :-
	write('       1 - Abstract Argumentation Framework (AF)'),nl, %Dung95
	write('       2 - Value-Based AF (VAF)'),nl,	%Bench-CaponEtAl2002
	write('       3 - Bipolar AF (BAF)'),nl,	%AmgoudEtAl 2004
	write('       4 - AF + Authority Degrees'),nl,	%Pazienza2015
	write('       5 - Weighted AF (WAF)'),nl,	%Dunne2011
	%write('       6 - Social AF (SAF)	%TODO'),nl,	%Leite2013
	%write('       7 - Abstract Dialectical Framework (ADF)	%TODO'),nl,	%Brewka2010
	%write('       8 - Preference-based AF (PAF)	%TODO'),nl,	%AmgoudCayrol2002
	%write('       9 - Extended AF (EAF)	%TODO'),nl,nl,	%Modgil2009
	write('       0 - Quit'),nl,nl,
	print('Enter your choice'),
	!.
