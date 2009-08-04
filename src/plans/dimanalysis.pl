%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file does elementary multiplication/division of units.

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- ensure_loaded('../declarations.pl').
:- ensure_loaded('conversion.pl').
:- rdf_meta multiply(t, t, t).
:- rdf_meta divide(t, t, t).
:- rdf_meta units(r, r).

/*
Find a UnitTypeURI for a product or ratio of two units.
*/
unit_type(ratio(NumeratorUnit, DenominatorUnit), UnitTypeURI) :-
	rdf(UnitTypeURI, gu:numeratorUnits, NumeratorUnit),
	rdf(UnitTypeURI, gu:denominatorUnits, DenominatorUnit),
	rdfs_individual_of(UnitTypeURI, gu:ratioUnits), !.

unit_type(product(UnitA, UnitB), UnitTypeURI) :-
	rdf(UnitTypeURI, gu:units, UnitA),
	rdf(UnitTypeURI, gu:units, UnitB),
	rdfs_individual_of(UnitTypeURI, gu:productUnits), !.

/*
Assert a new unit type for a ratio between units.
*/
unit_type(ratio(NumeratorUnit, DenominatorUnit), UnitTypeURI) :-
	rdf_transaction((
		rdf_node(UnitTypeURI),
		rdf_assert(UnitTypeURI, rdf:type, gu:ratioUnits),
		rdf_assert(UnitTypeURI, gu:numeratorUnits, NumeratorUnit),
		rdf_assert(UnitTypeURI, gu:denominatorUnits, DenominatorUnit)
	)).

/*
Assert a new unit type that is a product of two fundamental units
*/
unit_type(product(UnitA, UnitB), UnitTypeURI) :-
	rdf_transaction((
		rdf_node(UnitTypeURI),
		rdf_assert(UnitTypeURI, rdf:type, gu:productUnits),
		rdf_assert(UnitTypeURI, gu:units, UnitA),
		rdf_assert(UnitTypeURI, gu:units, UnitB)
	)).

/* multiply and divide quantities while retaining units */
multiply(units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueA)), UnitsA),
		 units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueB)), UnitsB),
		 units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', Result)), ResultUnits)) :-
	unit_type(product(UnitsA, UnitsB), ResultUnits),
	scale_factor(ResultUnits, Factor),
	to_om(Factor, FactorOOM),
	mult(ValueA, ValueB, Result1),
	mult(Result1, FactorOOM, Result).
	
multiply(units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueA)), UnitsA),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueB)), UnitsB),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', Result)), ResultUnits)) :-
	unit_type(product(UnitsA, UnitsB), ResultUnits),
	scale_factor(ResultUnits, Factor),
	Result is ValueA * ValueB * Factor.

divide(units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueN)), UnitsN),
	   units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueD)), UnitsD),
	   units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', Result)), ResultUnits)) :-
	unit_type(ratio(UnitsN, UnitsD), ResultUnits),
	scale_factor(ResultUnits, Factor),
	to_om(Factor, FactorOOM),
	div(ValueN, ValueD, Result1),
	mult(Result1, FactorOOM, Result).
	
divide(units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueN)), UnitsN),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueD)), UnitsD),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', Result)), ResultUnits)) :-
	unit_type(ratio(UnitsN, UnitsD), ResultUnits),
	scale_factor(ResultUnits, Factor),
	Result is (ValueN / ValueD) * Factor.
	
