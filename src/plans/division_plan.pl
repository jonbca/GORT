%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Plan for calculating the quantity of items that could go into
%% a certain measure.
:- use_module(library('semweb/rdf_db')).
:- ensure_loaded('../declarations').
:- ensure_loaded('dimanalysis').
:- rdf_meta quantity_of(r, r, r, r, -).

% Must be same dimension!
quantity_of(SmallerClass, SmallerPredicate, BiggerClass,
		BiggerPredicate, OutNode) :-
    fetch(SmallerClass, SmallerPredicate, ObjectNode, _),
    fetch(ObjectNode, rdf:value, literal(type(xsd:float, SmallerValue)), user),
    fetch(ObjectNode, gu:units, SmallerType, user),
    fetch(BiggerClass, BiggerPredicate, BiggerObjectNode, _),
    fetch(BiggerObjectNode, rdf:value, literal(type(xsd:float, BiggerValue)), _),
    fetch(BiggerObjectNode, gu:units, BiggerType, user),
    convert(SmallerValue, SmallerType, SmallerValueConverted, BiggerType),    %%FIXME
    to_om(SmallerValueConverted, SmallerValueConvertedOM),
    to_om(BiggerValue, BiggerValueOM),
    div(BiggerValueOM, SmallerValueConvertedOM, Guess),
    rdf_node(OutNode),
	rdf_assert(OutNode, rdf:type, gu:'ResultNode'),
	store_statement(OutNode, gu:result, literal(type(xsd:long, Guess)),
			gu:'GuesstimationTechnique', _OutVNode).
    
quantity_of(SmallerClass, BiggerClass, Predicate, OutNode) :-
	fetch(SmallerClass, Predicate, ObjectNode, _),
	fetch(ObjectNode, rdf:value, literal(type(xsd:float, SmallerValue)), user),
    fetch(ObjectNode, gu:units, SmallerType, user),
    fetch(BiggerClass, Predicate, BiggerObjectNode, _),
    fetch(BiggerObjectNode, rdf:value, literal(type(xsd:float, BiggerValue)), _),
    fetch(BiggerObjectNode, gu:units, BiggerType, user),
    convert(SmallerValue, SmallerType, SmallerValueConverted, BiggerType),    %%FIXME
    to_om(SmallerValueConverted, SmallerValueConvertedOM),
    to_om(BiggerValue, BiggerValueOM),
    div(BiggerValueOM, SmallerValueConvertedOM, Guess),
    rdf_node(OutNode),
	rdf_assert(OutNode, rdf:type, gu:'ResultNode'),
	store_statement(OutNode, gu:result, literal(type(xsd:long, Guess)),
			gu:'GuesstimationTechnique', _OutVNode).