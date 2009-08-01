%%%% 
%% $Id$
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
:- rdf_register_ns(fb, 'http://www4.wiwiss.fu-berlin.de/factbook/ns#').

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
:- ensure_loaded('plans/geographical').