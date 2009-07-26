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

:- write('Loading semantic web modules'), nl.

:- use_module(library('semweb/rdf_turtle')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_cache:rdf_set_cache_options([enabled(true)]).

%% namespace prefixes
:- rdf_register_ns(wni, 'http://www.w3.org/2006/03/wn/wn20/instances/').
:- rdf_register_ns(wns, 'http://www.w3.org/2006/03/wn/wn20/schema/').
:- rdf_register_ns(gu, 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/').
:- rdf_register_ns(dbo, 'http://dbpedia.org/ontology/').
:- rdf_register_ns(dbp, 'http://dbpedia.org/property/').
:- rdf_register_ns(dc, 'http://purl.org/dc/elements/1.1/').
:- rdf_register_ns(geo, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- write('Loading guesstimation program'), nl.

:- ensure_loaded('ui/exceptionlogging').
:- ensure_loaded('plans/epsilon').
:- ensure_loaded('meta/rdfutils').
:- ensure_loaded('meta/cycutils').
:- ensure_loaded('oom/oom-updated').
:- ensure_loaded('inference/owl2').
:- ensure_loaded('plans/wordnet_meronymy').
:- ensure_loaded('plans/conversion').
:- ensure_loaded('plans/fetch').
:- ensure_loaded('plans/relativesize').
:- ensure_loaded('plans/population-geo').

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

:- write('Fixing OpenCyc - Wordnet Mappings'),nl.
:- fix_ocyc_synsets.

:- rdf_set_predicate(owl:sameAs, transitive(true)).
:- rdf_set_predicate(owl:sameAs, symmetric(true)).

:- write('Loaded.'),nl.