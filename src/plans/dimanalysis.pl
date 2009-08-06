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
:- rdf_meta unit_type(t, t, -).
:- rdf_meta product(r, r).
:- rdf_meta ratio(r, r).

/*
Find a UnitTypeURI for a product or ratio of two units.
*/
unit_type(ratio(NumeratorUnit, DenominatorUnit), UnitTypeURI) :-
	rdf(UnitTypeURI, gu:numeratorUnits, NumeratorUnit),
	rdf(UnitTypeURI, gu:denominatorUnits, DenominatorUnit),
	rdfs_individual_of(UnitTypeURI, gu:ratioUnits), !.

unit_type(product(UnitA, UnitA), UnitTypeURI) :-
	area_units_for_length(UnitA, UnitTypeURI), !.
	
unit_type(product(UnitA, UnitB), UnitTypeURI) :-
	area_units_for_length(UnitA, UnitB),
	volume_units_for_length(UnitA, UnitTypeURI), !.

unit_type(product(UnitA, UnitB), UnitTypeURI) :-
	rdf_has(UnitTypeURI, gu:units, UnitA),
	rdf_has(UnitTypeURI, gu:units, UnitB),
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
		rdf_assert(UnitTypeURI, gu:leftUnits, UnitA),
		rdf_assert(UnitTypeURI, gu:rightUnits, UnitB)
	)).
/*
simplify(UnitTypeURI, ScaleFactor, SimplifiedURI) :-
	flatten_units(UnitTypeURI, NumeratorUnits, DenominatorUnits).


flatten_units(Units, [Units]) :-
	\+rdfs_individual_of(Units, gu:compoundUnits).

flatten_units(Units, [Nh, Nt], [Dh | Dt]) :-
	rdfs_individual_of(Units, gu:productUnits),
	rdf_has(Units, gu:leftUnits, Left),
	rdf_has(Units, gu:rightUnits, Right),
	flatten_units(Left, Nh, _),
	flatten_units(Right, Nt, _).

flatten_units(Units, [Nh | Nt], [Dh | Dt]) :-
	rdfs_individual_of(Units, gu:ratioUnits),
	rdf_has(Units, gu:numeratorUnits, Num),
	rdf_has(Units, gu:denominatorUnits, Den),
	flatten_units(Num, Nh, [Dh | Dt]),
	flatten_units(Den, [Nh | Nt], Dh). 
*/

/* multiply and divide quantities while retaining units */
multiply(units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueA)), UnitsA),
		 units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueB)), UnitsB),
		 units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', Result)), ResultUnits)) :-
	unit_type(product(UnitsA, UnitsB), ResultUnits),
	mult(ValueA, ValueB, Result).

multiply(units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueA)), UnitsA),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueB)), UnitsB),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', Result)), ResultUnits)) :-
	(number(ValueA) -> ValueAN = ValueA; atom_number(ValueA, ValueAN)),
	(number(ValueB) -> ValueBN = ValueB; atom_number(ValueB, ValueBN)),
	unit_type(product(UnitsA, UnitsB), ResultUnits),
	Result is ValueAN * ValueBN.

divide(units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueN)), UnitsN),
	   units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', ValueD)), UnitsD),
	   units(literal(type('http://www.inf.ed.ac.uk/2009/06/01/guesstimation/oom', Result)), ResultUnits)) :-
	unit_type(ratio(UnitsN, UnitsD), ResultUnits),
	div(ValueN, ValueD, Result).
	
divide(units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueN)), UnitsN),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', ValueD)), UnitsD),
		 units(literal(type('http://www.w3.org/2001/XMLSchema#float', Result)), ResultUnits)) :-
	(number(ValueN) -> ValueNN = ValueN; atom_number(ValueN, ValueNN)),
	(number(ValueD) -> ValueDN = ValueD; atom_number(ValueD, ValueDN)),
	unit_type(ratio(UnitsN, UnitsD), ResultUnits),
	Result is ValueNN / ValueDN.
	
portray(units(Subject)) :-
	rdfs_individual_of(Subject, gu:ratioUnits),
	once(rdf(Subject, gu:numeratorUnits, NumUnits)),
	once(rdf(Subject, gu:denominatorUnits, DemUnits)),
	print('('), print(units(NumUnits)), print(' / '), print(units(DemUnits)), print(')').
	
portray(units(Subject)) :-
	rdfs_individual_of(Subject, gu:productUnits),
	findall(U, rdf_has(Subject, gu:units, U), Units),
	print('('), print_all_units(Units), print(')').

portray(units(Subject)) :-
	solve(Subject, gu:prologSymbol, [literal(Symbol) |_]), !,
	print(Symbol).
	
portray(units(Subject)) :-
	cyclify(Subject, EnglishName),
	print(EnglishName).

print_all_units([]).
print_all_units([H | []]) :-
	print(units(H)), !.
print_all_units([H | T]) :-
	print(units(H)), print(' * '), print_all_units(T).