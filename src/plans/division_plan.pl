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
:- rdf_meta quantity_of(r, r, r, r, -).

% Must be same dimension!
quantity_of(SmallerClass, SmallerPredicate, BiggerClass,
		BiggerPredicate, literal(type(gu:oom, Guess))) :-
    fetch(SmallerClass, SmallerPredicate, ObjectNode, _),
    fetch(ObjectNode, rdf:value, literal(type(xsd:float, SmallerValue)), user),
    fetch(ObjectNode, gu:units, SmallerType, user),
    fetch(BiggerClass, BiggerPredicate, BiggerObjectNode, _),
    fetch(BiggerObjectNode, rdf:value, literal(type(xsd:float, BiggerValue)), _),
    fetch(BiggerObjectNode, gu:units, BiggerType, user),
    convert(SmallerValue, SmallerType, SmallerValueConverted, BiggerType),    %%FIXME
    to_om(SmallerValueConverted, SmallerValueConvertedOM),
    to_om(BiggerValue, BiggerValueOM),
    div(BiggerValueOM, SmallerValueConvertedOM, Guess).
    
quantity_of(SmallerClass, BiggerClass, Predicate, literal(type(gu:oom, Guess))) :-
	fetch(SmallerClass, Predicate, ObjectNode, _),
	fetch(ObjectNode, rdf:value, literal(type(xsd:float, SmallerValue)), user),
    fetch(ObjectNode, gu:units, SmallerType, user),
    fetch(BiggerClass, Predicate, BiggerObjectNode, _),
    fetch(BiggerObjectNode, rdf:value, literal(type(xsd:float, BiggerValue)), _),
    fetch(BiggerObjectNode, gu:units, BiggerType, user),
    convert(SmallerValue, SmallerType, SmallerValueConverted, BiggerType),    %%FIXME
    to_om(SmallerValueConverted, SmallerValueConvertedOM),
    to_om(BiggerValue, BiggerValueOM),
    div(BiggerValueOM, SmallerValueConvertedOM, Guess).
    
quantity_of_div(TopClass, TopPredicate, BottomClass, BottomPredicate, Out) :-
	fetch(TopClass, TopPredicate, TopResult),
	fetch(BottomClass, BottomPredicate, BottomResult),
	rdf(TopResult, gu:units, TopUnits),
	rdf(BottomResult, gu:units, BottomUnits),
	rdf(TopResult, rdf:value, literal(type(xsd:float, TopResultN))),
	rdf(BottomResult, rdf:value, literal(type(xsd:float, BottomResultN))),
	divide(units(literal(type(xsd:float, TopResultN)), TopUnits),
		units(literal(type(xsd:float, BottomResultN)), BottomUnits),
		Out).