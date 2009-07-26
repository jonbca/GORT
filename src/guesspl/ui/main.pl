%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Main UI loop

:- dynamic current_user/1.

:- use_module(library(readutil)).
main :-
	writeln('Semi-automatic guesstimation'),
	repeat,
	do_command,
	!.
	
do_command :-
	prompt(_, 'guess> '),
	read_input_line(Tokens),
	command(Clist, Tokens, []),
	Command =.. Clist,
	call(Command),
	!,
	Command == exit,
	writeln('Bye.').



    
command([Operation | Arguments]) --> operation(Operation), arguments(Arguments).

arguments([Arg | Args]) --> argument(Arg), arguments(Args).
arguments([]) --> [].

operation(guess_total) --> [findtotal].
operation(exit) -->      [exit]
                   |     [quit]
                   |     [bye]
                   |     [halt].
                   
operation(guess_tell) --> [tell].
operation(guess_abolish) --> [reset] | [abolish].
operation(guess_retract) --> [untell] | [retract].
operation(guess_query) --> [ask].
operation(ui_population) --> [population].
operation(guess_show) --> [show].

argument(Concept) --> [Concept], {cyclify(_, Concept)}.
argument(Value) --> [Value].

measure(mass) --> [mass].
measure(length) --> [length].

guess_total(_).
guess_total(_, _).

exit.