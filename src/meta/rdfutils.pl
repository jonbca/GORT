%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Reification for RDF statements

:- use_module(library('semweb/rdf_db')).
:- rdf_meta reify_create(t, r, o, -), reification(t, r, o, ?).

/** Creates a reification for an RDF node. The RDF node must exist, but this
goal does not check for its existence. The StatementId unifies with the URI
for the newly-created statement
*/
reify_create(Triple, PredicateR, ObjectR:_, StatementId) :-
	reify_create( Triple, PredicateR, ObjectR, StatementId), !.

reify_create(rdf(Subject, Predicate, Object), PredicateR, ObjectR, StatementId) :-
    \+var(Subject), \+var(Predicate), \+var(Object), \+var(PredicateR), \+var(ObjectR),
	var(StatementId),
	rdf_node(StatementId),
	rdf_assert(StatementId, rdf:type, rdf:'Statement'),
	rdf_assert(StatementId, rdf:subject, Subject),
	rdf_assert(StatementId, rdf:predicate, Predicate),
	rdf_assert(StatementId, rdf:object, Object),
	rdf_assert(StatementId, PredicateR, ObjectR).

/** Updates a reification with a new property for a statement. The Statement
must already exist, but this goal does not check for its existence. */
reify_update(rdf(Subject, Predicate, Object), PredicateR, ObjectR, StatementId) :-
    \+var(Subject), \+var(Predicate), \+var(Object), \+var(PredicateR), \+var(ObjectR),
    \+var(StatementId),
    rdf_assert(StatementId, rdf:type, rdf:'Statement'),
	rdf_assert(StatementId, rdf:subject, Subject),
	rdf_assert(StatementId, rdf:predicate, Predicate),
	rdf_assert(StatementId, rdf:object, Object),
	rdf_assert(StatementId, PredicateR, ObjectR).

/** Retrieves the triples for the reification of the RDF triple specified as the
first argument */
reification(rdf(Subject, Predicate, Object), PredicateR, ObjectR, StatementId) :-
    rdf(StatementId, rdf:type, rdf:'Statement', user),
    rdf(StatementId, rdf:subject, Subject, user),
	rdf(StatementId, rdf:predicate, Predicate, user),
	rdf(StatementId, rdf:object, Object, user),
	rdf(StatementId, PredicateR, ObjectR, user).