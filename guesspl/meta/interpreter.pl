%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains an interpreter for generating proof trees. It is
%% partially based on the meta-interpreters presented in Bratko (2001) and
%% Sterling (1994).

:- include(operators).
:- dynamic noprove/1.
:- multifile noprove/1.
:- dynamic proving/0.
:- multifile silent/1.

%% noprove(+Goal) is det.
%
%  Identify clauses that should not be interpreted. These clauses may contain
%  logic, as below, or may just be simple facts with the name of the predicate,
%  e.g.:
%
%  ==
%  noprove(foo).
%  ==
%
%  This is a multifile dynamic predicate. noprove/1 clauses should be included in
%  the file in which the clause contained in Goal is declared.
%
%  @param Goal the clause for which no proof should be presented by the meta-interpreter.
%
noprove(Goal) :-
    predicate_property(Goal, built_in).
    
noprove(Goal) :-
    predicate_property(Goal, imported_from(_)).
    
noprove(Goal) :-
    predicate_property(Goal, foreign).

%% suspend_proof(+Goal) is det.
%
%  Check to see if a noprove/1 fact exists for the given goal.
%  This goal succeeds if the interpreter should NOT list a detailed
%  proof for the given goal.
%
%  @param Goal the goal to check
suspend_proof(Goal) :-
    \+var(Goal),
    noprove(Goal),
    !.

suspend_proof(Goal) :-
    \+var(Goal),
    Goal =.. [Pred | _],
    noprove(Pred),
    !.

%% prove(+Goal) is nondet.
%  Constructs a proof tree for Goal. The format of the proof tree is
%  as follows:
%
%  $ Call : Indicates that a given goal is being called with the arguments shown.
%  $ Yes : Indicates that the goal completed successfully. Any previously unbound
%          terms are shown with their bound values.
%  $ Negation : Indicates that a given goal is to be negated (i.e. appears in a not/1 clause).
%  $ False : Indicates that the given goal is false, and is part of a negation. In other words, the negation of the goal succeeded.
%  $ RDF Query : Indicates that the given goal is an RDF query and is presented in a special syntax.
%  $ Derived Fact : Indicates that the goal is a fact that has been derived by following equivalence relations over the RDF graph.
%  $ Call (Proof Suspended) : Indicates that the goal is being called, but proof printing has been suspended by the presence of a noprove/1 fact.
%  $ Redo : The given goal has more solutions and is being backtracked.
%
%  @param Goal the goal to be proven.
prove(Goal) :- nl, write('-----'), nl, prove(Goal, 0).

% Cut on finding a proof.
prove(true, _) :-
    !.

% Prove conjunction
prove((Goal1, Goal2), Depth) :-
    !,
    prove(Goal1, Depth),
    prove(Goal2, Depth).

%prove((Goal1 -> Goal2 ; Goal3), Depth) :-
%    !,
%    (prove(Goal1, Depth) -> prove(Goal2, Depth) ; prove(Goal3, Depth)).

%Prove negated goals, but show proof path
prove(not(Goal), Depth) :-
    display('Negation: ', Goal, Depth),
    Depth1 is Depth + 1,
    ( prove(Goal, Depth1) -> fail ; true, ! ),
    display('False: ', Goal, Depth).

% Prove predicates of the form rdf(S, P, O). This outputs a more readable
% version of the RDF fact.
prove(rdf(S, P, O), Depth) :-
    display('RDF query: ', rdf(S, P, O), Depth),
    rdf(S, P, O),
    display('Fact: ', rdf(S, P, O), Depth).

prove(solve(S, P, [O | Trail]), Depth) :-
    !, solve(S, P, [O | Trail]),
    display('Derived Fact: ', rdf(S, P, O), Depth),
    display_trace( Trail, Depth ).

% Prove goals of the form solve(S, P, O). These goals also produce a trace of
% the depth-first search of the RDF tree.
%prove(solve(S, P, [O | Trail]), Depth) :-
%    solve(S,P,O),
%    display('Derived Fact: ', rdf(S, P, O), Depth),
%    display_trace( Trail, Depth ).

% Execute silent predicates (i.e. logging)
prove(Goal, _) :-
    Goal =.. [Pred | _],
    silent(Pred), !, 
    Goal.

% Prove predicates we don't want to itemize a solution for
prove(Goal, Depth) :-
    not( Goal =.. [rdf | _] ),   % this predicate is handled as a special case
    not( Goal =.. [not | _] ),   % this predicate is handled as a special case
    suspend_proof(Goal), 
    display('Call (Proof suspended): ', Goal, Depth),
    Goal,
    display('Yes: ', Goal, Depth),
    display_redo(Goal, Depth).   % backtrack

% Prove my regular interpreted Prolog goals in source files
prove(Goal, Depth) :-
    not( Goal =.. [solve | _] ), % this predicate is handled as a special case
    not( suspend_proof(Goal) ),
	clause(Goal, Body),
	display('Call: ', Goal, Depth), 
    Depth1 is Depth + 1,
    prove(Body, Depth1),
    display('Yes: ', Goal, Depth),
    display_redo(Goal, Depth).   % backtrack
    
% When all else fails...
prove(Goal, Depth) :-
    display( 'Fail: ', Goal, Depth),
    fail.

% MESSAGE PRINTING

% Display a message, indented at the appropriate proof level
display(Message, Goal, Depth) :-
    Indent is Depth * 4,
    tab(Indent), write(Message), 
    write_term(Goal, [max_depth(3), quoted(true)]), nl.

% Display the trace from a solve/3 predicate
display_trace([], _).

display_trace([Head | Trail], Depth) :-
    tab(Depth), write( ==> Head ), nl,
    Depth1 is Depth + 2,
    display_trace(Trail, Depth1).

% Show backtracking, if appropriate
display_redo(Goal, Depth) :-
    true
    ;
    display('Redo: ', Goal, Depth),
    fail.