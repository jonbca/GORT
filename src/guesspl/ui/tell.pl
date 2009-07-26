%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains predicates for updating the RDF database with a local
%% customized ontology for doing inference on a specific problem.

guess_tell(Subject, Predicate, Object) :-
    cyclify(SubjectURI, Subject),
    cyclify(PredicateURI, Predicate),
    rdf_assert(SubjectURI, PredicateURI, literal(Object)),
    print_msg('Told: ', [Subject, Predicate, Object]).
    
guess_abolish :-
    rdf_transaction(
    rdf_retractall(_, _, _, user)
    ),
    print_msg('Reset database.', []).
    
guess_retract(Subject, Predicate, Object) :-
    cyclify(SubjectURI, Subject),
    cyclify(PredicateURI, Predicate),
    rdf_retractall(SubjectURI, PredicateURI, literal(Object)),
    print_msg('Untold: ', [Subject, Predicate, Object]).
    
guess_persist :-
    rdf_save('persisted_user.rdf', [graph(user)]).

guess_query(Subject, Predicate) :-
    cyclify(SubjectURI, Subject),
    cyclify(PredicateURI, Predicate),
    rdf_has(SubjectURI, PredicateURI, Object),
    print_msg('Asked: ', [Subject, Predicate, Object]).

guess_query(Subject, Predicate) :-
    \+rdf_has(Subject, Predicate, _),
    print_msg('No.', []).

guess_forname(Name, URI) :-
    rdf_has(URI, rdfs:label, literal(Name)).

guess_show :-
    rdf(Subj, Pred, Obj, user),
    print_msg('Custom: ', [rdf(Subj, Pred, Obj)]).
    
print_msg(Message, Args) :-
    write(Message),
    print_args(Args),
    nl.

print_args([]).
print_args([H | T]) :-
    write( H ),
    write( ' ' ),
    print_args(T).