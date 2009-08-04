%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Reification for RDF statements

:- use_module(library('semweb/rdf_db')).
:- ensure_loaded('../declarations.pl').
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
	
print_value_node(NodeURI) :-
	write(' has value '),
	rdf(NodeURI, rdf:value, literal(type(xsd:float, FloatValue))),
	write(FloatValue),
	rdf(NodeURI, rdf:value, literal(type(gu:oom, OOMValue))),
	write(' or '),
	write(OOMValue),
	rdf(NodeURI, gu:units, Units),
	once(symbol_uri(Symbol, Units)),
	write(' '), write(Symbol), nl.

/** dump_customised_ontology is multi.
Print out all statements in the customised ontology in a user-friendly way.

*/
dump_customised_ontology :-
	findall(Subj, get_statements(Subj), Subjects),
	sort(Subjects, SubjectSet),
	print_nodes(SubjectSet, _).

get_statements(Subj) :-
	rdf(State, rdf:type, rdf:'Statement'),
	rdf(State, rdf:subject, Subj).

print_nodes([X | _], X) :-
	print_node(X).
print_nodes([_ | Tail], X) :-
	print_nodes(Tail, X).

/**
print_node/1
Print out a description of an epsilon node
*/
print_node(Subj) :-
	rdf(Subj, rdf:type, gu:'Epsilon', user),
	rdf(Subj, Pred, Obj, user),
	Pred \= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	once((rdf(Subj, rdf:type, Class),
    	Class \= 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/Epsilon')),
    write('Typical of class '),
    (cyclify(Class, EnglishClass) -> true ; Class = EnglishClass),
    write(EnglishClass),
    write(' for '),
    (once(rdf(Pred, rdfs:label, literal(EnglishPred))) -> true ; Pred = EnglishPred),
    write(EnglishPred),
    print_value_node(Obj), 
    reification(rdf(Subj, Pred, Obj), dc:source, Source, _),
    write('      source: '),
    ( once(rdf(Source, rdfs:label, literal(lang(en, Label)))) -> true ; Source = Label ),
    write(Label), nl.
    
/** print_node/1
Print out a description of a non-epsilon node.
*/
print_node(Subj) :-
	\+rdf(Subj, rdf:type, gu:'Epsilon', user),
	rdf(Subj, Pred, Obj, user),
	Pred \= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	(once(rdf(Subj, rdfs:label, literal(EnglishSubj))) -> true ; Subj = EnglishSubj),
	write(EnglishSubj),
	write(' for '),
	(once(rdf(Pred, rdfs:label, literal(EnglishPred))) -> true ; Pred = EnglishPred),
	write(EnglishPred),
	print_value_node(Obj),
	reification(rdf(Subj, Pred, Obj), dc:source, Source, _),
    write('      source: '),
    ( once(rdf(Source, rdfs:label, literal(lang(en, Label)))) -> true ; Source = Label ),
    write(Label), nl.