%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for mapping Cyc URIs to English names.

:- ensure_loaded('interpreter').
:- use_module(library('semweb/rdf_db')).

:- multifile noprove/1.

noprove(cyclify).  % trivial clause, and includes cut

%% Register namespaces for cyc, opencyc, and others required for
%  Cyclification
:- rdf_db:rdf_register_ns(cycAnnot, 'http://sw.cyc.com/CycAnnotations_v1#').
:- rdf_db:rdf_register_ns(ocyc, 'http://sw.opencyc.org/concept/').
:- rdf_db:rdf_register_ns(cyc, 'http://sw.cyc.com/concept/').
:- rdf_db:rdf_register_ns(skos, 'http://www.w3.org/2004/02/skos/core#').

%% cyclify/2 unifies CycURI with the URI for a Cyc concept, and EnglishName
%  unifies with the label for a Cyc concept. This goal is used to find
%  either the English name for a concept, or the URI for a given English concept
%  name.
cyclify(CycURI, EnglishName) :-
    rdf(CycURI, cycAnnot:label, literal(lang(_, EnglishName))), !. % do not want more than 1 result

%% cyc_consult/3 consults the knowledge base with English subject, predicate, and
%  object. This goal efficiently executes if a Subject is given and either the
%  English predicate or English object are being searched for.
cyc_consult(EnglishSubject, EnglishPredicate, EnglishObject) :-
    cyclify(SubjectURI, EnglishSubject),
    rdf(SubjectURI, PredicateURI, ObjectURI),
    cyclify(PredicateURI, EnglishPredicate),
    cyclify(ObjectURI, EnglishObject).
    
remove_outdated_ocyc_dbpedia :-
    rdf_transaction(retract_triples, _).

retract_triples :-
    dbpedia_triple(CycTriple, DBPediaTriple),
    rdf_retractall(CycTriple, owl:'sameAs', DBPediaTriple),
    fail; true.

dbpedia_triple(CycTriple, URI) :-
    rdf(CycTriple, owl:'sameAs', URI),
    rdf_split_url('http://dbpedia.org/resource/', _, URI).