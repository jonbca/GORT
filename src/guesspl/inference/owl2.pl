%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for searching an RDF graph for triples
%% that satisfy a predicate, based on a starting node. Searches
%% according to owl:sameAs relationships.

:- use_module(library('semweb/rdf_db')).

:- multifile noprove/1.

:- rdf_meta solve(r, r, -), solve(r, r, -, -).

noprove(solve).    % too verbose

/* solve(+Start, +Predicate, ?Solution) is multi.
Solve takes a Start node and target Predicate, and returns a solution,
following transitive/symmetric/reflexive owl:sameAs relations. Returns
the literal at the end of the solution path, and the actual node
that contains the literal.

@param Start the start node
@param Predicate the predicate to search for
@param Solution the solution
*/
solve(Start, Predicate, [Literal, Answer]) :-
	get_answers(Start, AllAnswers),
	member(Answer, AllAnswers),
	rdf_has(Answer, Predicate, Literal).
	
solve(Start, Predicate, [Literal, Answer], Graph) :-
	get_answers(Start, AllAnswers),
	member(Answer, AllAnswers),
	rdf_has(Answer, Predicate, Literal, ActualPredicate),
	rdf(Answer, ActualPredicate, Literal, Graph).
	
get_answers(Start, AllAnswers) :-
	findall(AnswerF, rdf_reachable(Start, owl:sameAs, AnswerF), AnswersF),
	findall(AnswerB, rdf_reachable(AnswerB, owl:sameAs, Start), AnswersB),
	union(AnswersF, AnswersB, AllAnswers).