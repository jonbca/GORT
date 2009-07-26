%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Plan for calculating the quantity of items that could go into
%% a certain measure.

quantity_of(SmallerClass, SmallerPredicate, BiggerClass,
		BiggerPredicate, literal(type(xsd:float, Guess))) :-
    fetch(SmallerClass, SmallerPredicate, ObjectNode, _),
    fetch(ObjectNode, rdf:value, literal(type(xsd:float, SmallerValue)), user),
    fetch(ObjectNode, gu:units, SmallerType, user),
    fetch(BiggerClass, BiggerPredicate, BiggerObjectNode, _),
    fetch(BiggerObjectNode, rdf:value, literal(type(xsd:float, BiggerValue)), _),
    fetch(BiggerObjectNode, gu:units, BiggerType, user),
    convert(SmallerValue, SmallerType, SmallerValueConverted, BiggerType),
    to_om(SmallerValueConverted, SmallerValueConvertedOM),
    to_om(BiggerValue, BiggerValueOM),
    div(BiggerValueOM, SmallerValueConvertedOM, Guess).