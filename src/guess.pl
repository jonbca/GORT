%%%% 
%% $Id: guess.pl 146 2009-07-06 11:43:19Z jabourbih $
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file loads necessary Prolog files, modules, and RDF data sources into
%% Prolog.

:- dynamic noprove/1.
:- multifile noprove/1.
:- ensure_loaded('declarations').

:- write('Loading semantic web modules'), nl.

:- use_module(library('semweb/rdf_turtle')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- write('Loading OpenCyc.'),nl.

:- rdf_db:rdf_load('../guessdata/opencyc-latest.owl').

:- write('Removing outdated OpenCyc to DbPedia mappings'), nl.
:- remove_outdated_ocyc_dbpedia.

:- write('Loading remaining RDF ontologies.').
:- rdf_db:rdf_load('../guessdata/fake-pop-data.ttl').
:- rdf_db:rdf_load('../guessdata/owl.owl').
:- rdf_db:rdf_load('../guessdata/factbook.rdf').
:- rdf_db:rdf_load('../guessdata/infoboxproperties_en.ttl').
:- rdf_db:rdf_load('../guessdata/links_factbook_en.ttl').
:- rdf_db:rdf_load('../guessdata/dbpedia-ontology.owl').
:- rdf_db:rdf_load('../guessdata/links_opencyc_en.ttl').
:- rdf_db:rdf_load('../guessdata/geo_en.ttl').

:- write('Loading wordnet'),nl.

%% Load Wordnet
:-rdf_db:rdf_load('../guessdata/wordnet/wnbasic.rdfs').
:-rdf_db:rdf_load('../guessdata/wordnet/wordnet-partmeronym.rdf').
:-rdf_db:rdf_load('../guessdata/wordnet/wordnet-synset.rdf').

% Load DBPedia Extracts
:-rdf_db:rdf_load('../guessdata/dbpedia_extracts/infobox-runtime.ttl').
:-rdf_db:rdf_load('../guessdata/dbpedia_extracts/infobox-height.ttl').
:-rdf_db:rdf_load('../guessdata/dbpedia_extracts/infobox-weight.ttl').
:-rdf_db:rdf_load('../guessdata/dbpedia_extracts/infobox-width.ttl').
:-rdf_db:rdf_load('../guessdata/dbpedia_extracts/earth.ttl').

:- write('Fixing OpenCyc - Wordnet Mappings'),nl.
:- fix_ocyc_synsets.
:- map_operators.

:- rdf_set_predicate(owl:sameAs, transitive(true)).
:- rdf_set_predicate(owl:sameAs, symmetric(true)).

:- write('Loaded.'),nl.