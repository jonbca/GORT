%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Plan for calculating the 'size' of something based on a typical value
%% for that something.

:- use_module(library('semweb/rdf_db')).

:- rdf_meta total_size(r, r, r, r, -).

total_size(Class, Parameter, MagSubject, MagPredicate, literal(type(Type, Guess))) :-
    fetch(Class, Parameter, ObjectNode, _),   % Fetch the resource whose size we need
    fetch(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM)), user), % Pull out the OM value
    fetch(ObjectNode, gu:units, Type, user),  % Pull out the type and such
    magnitude(MagSubject, MagPredicate, MagnitudeOM),
	mult(ValueOM, MagnitudeOM, Guess).
	
magnitude(Class, Predicate, Magnitude) :-
    fetch(Class, Predicate, ObjectNode, _),
    fetch(ObjectNode, rdf:value, literal(type(gu:oom, Magnitude)), user).