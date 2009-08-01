%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains rewrite rules for calculating volumes, areas, lengths, etc.

:- ensure_loaded('conversion').
:- ensure_loaded('../declarations').
:- rdf_meta s_fetch(r, r, r, -).

s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVjbrZwpEbGdrcN5Y29ycA'), % volumeOfObject
	!,
	fetch(Subject, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA', ObjectNodeD, _), % get diameter
	rdf(ObjectNodeD, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeD, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	VolumeN is (4/3) * pi * (N ** 3), % calculate surface area
	volume_units_for_length(Units, VolumeUnits),
	store_statement(Subject, Predicate, literal(type(VolumeUnits, VolumeN)), gu:'Geometry', ObjectNode).
	

%Circumference from diameter
s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVigYJwpEbGdrcN5Y29ycA'), % circumferenceOfObject
	!,
	fetch(Subject, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA', ObjectNodeD, _), % get diameter
	rdf(ObjectNodeD, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeD, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	CircumferenceN is pi * N, % calculate surface area
	store_statement(Subject, Predicate, literal(type(Units, CircumferenceN)), gu:'Geometry', ObjectNode).

% Area from Diameter for sphere
s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVifGJwpEbGdrcN5Y29ycA'), % areaOfObject
	!,
	fetch(Subject, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA', ObjectNodeD, _), % get diameter
	rdf(ObjectNodeD, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeD, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	AreaN is pi * N ** 2, % calculate surface area
	area_units_for_length(Units, AreaUnits),
	store_statement(Subject, Predicate, literal(type(AreaUnits, AreaN)), gu:'Geometry', ObjectNode).

% Area from diameter for circle
s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_individual_of(Subject, ocyc:'Mx4rC1k_6q00EdmAAAACs0uFOQ'), % TwoDimensionalThing
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVifGJwpEbGdrcN5Y29ycA'), % areaOfObject
	!,
	fetch(Subject, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA', ObjectNodeD, _), % get diameter
	rdf(ObjectNodeD, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeD, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	AreaN is pi * (N/2) ** 2, % calculate surface area
	area_units_for_length(Units, AreaUnits),
	store_statement(Subject, Predicate, literal(type(AreaUnits, AreaN)), gu:'Geometry', ObjectNode).

% Diameter from Radius for round thing
s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA'), % Diameter
	!,
	fetch(Subject, ocyc:'Mx4rvVi6yZwpEbGdrcN5Y29ycA', ObjectNodeR, _), % get radius
	rdf(ObjectNodeR, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeR, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	Diameter is 2 * N,
	store_statement(Subject, Predicate, literal(type(Units, Diameter)), gu:'Geometry', ObjectNode).


% Plans for calculating stuff
volume(sphere, radius(Length), Volume) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    VolumeN is (4 / 3) * pi * (N ** 3),
    Volume =.. [Symbol, VolumeN].

volume(sphere, diameter(Length), Volume) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    VolumeN is (4/3) * pi * ((N/2) ** 3),
    Volume =.. [Symbol, VolumeN].

perimeter(circle, radius(Length), Perimeter) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    PerimeterN is pi * 2 * N,
    Perimeter =.. [Symbol, PerimeterN].

perimeter(circle, diameter(Length), Perimeter) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    PerimeterN is pi * N,
    Perimeter =.. [Symbol, PerimeterN].

area(circle, radius(Length), Area) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    AreaN is pi * N ** 2,
    area_units_for_length(Symbol, AreaUnit),
    Area =.. [AreaUnit, AreaN].

area(circle, diameter(Length), Area) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    AreaN is pi * (N / 2) ** 2,
    area_units_for_length(Symbol, AreaUnit),
    Area =.. [AreaUnit, AreaN].

area(sphere, diameter(Length), Area) :-
	length_unit(Length),
	Length =.. [Symbol, N],
	AreaN is pi * N ** 2,
	area_units_for_length(Symbol, AreaUnit),
	Area =.. [AreaUnit, AreaN].

area(square, Length, Area) :-
    length_unit(Length),
    Length =.. [Symbol, N],
    AreaN is N * N,
    area_units_for_length(Symbol, AreaUnit),
    Area =.. [AreaUnit, AreaN].
    
area(rectangle, Length, Width, Area) :-
    Width =.. [SymbolWidth, W],
    ConvertedLength =.. [SymbolWidth, L],
    convert(Length, ConvertedLength),
    AreaN is L * W,
    area_units_for_length(SymbolWidth, AreaUnit),
    Area =.. [AreaUnit, AreaN].
    
    
% Find area/volume units for a given length unit.
% This is a particularly messy way of doing it, and relies
% on the symbols defined in the ontology. It would be better
% to determine these based on the ontology.
area_units_for_length(LengthURI, AreaURI) :-
	symbol_uri(LengthUnit, LengthURI),
    atom_concat('sq_', LengthUnit, AreaUnit),
    symbol_uri(AreaUnit, AreaURI), !.
    
volume_units_for_length(LengthURI, VolumeURI) :-
	symbol_uri(LengthUnit, LengthURI),
    atom_concat('cu_', LengthUnit, VolumeUnit),
    symbol_uri(VolumeUnit, VolumeURI), !.