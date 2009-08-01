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

%%%%% Plans for round things %%%%%%
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
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvveoTpwpEbGdrcN5Y29ycA'), % surfaceAreaOfWholeObject
	!,
	fetch(Subject, ocyc:'Mx4rvVjaGJwpEbGdrcN5Y29ycA', ObjectNodeD, _), % get diameter
	rdf(ObjectNodeD, rdf:value, literal(type(xsd:float, Value))),
	rdf(ObjectNodeD, gu:units, Units),
	!,
	( number(Value) -> N = Value ; atom_number(Value, N) ),
	AreaN is pi * N ** 2, % calculate surface area
	area_units_for_length(Units, AreaUnits),
	store_statement(Subject, Predicate, literal(type(AreaUnits, AreaN)), gu:'Geometry', ObjectNode).

% Area from diameter for "top" of sphere (i.e. sphere projection onto 2d surface)
s_fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rwQVz15wpEbGdrcN5Y29ycA'), % surfaceAreaOfTopOfObject
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

%%%%% Plans for ... not? round things ... %%%%%
s_fetch(Subject, Predicate, ObjectNode, user) :-
	\+rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rwQVz15wpEbGdrcN5Y29ycA'), % surfaceAreaOfTopOfObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	!,
	fetch(Subject, ocyc:'Mx4rvVjbZpwpEbGdrcN5Y29ycA', LengthNode, _), % lengthOfObject
	fetch(Subject, ocyc:'Mx4rvVjgA5wpEbGdrcN5Y29ycA', WidthNode, _), % widthOfObject
	%% Extract length and width for object
	rdf(LengthNode, rdf:value, literal(type(xsd:float, Length))),
	rdf(LengthNode, gu:units, LengthUnits),
	rdf(WidthNode, rdf:value, literal(type(xsd:float, Width))),
	rdf(WidthNode, gu:units, WidthUnits),
	!,
	%% Make sure we have numbers
	( number(Length) -> LengthN = Length ; atom_number(Length, LengthN) ),
	( number(Width) -> WidthN = Width ; atom_number(Width, WidthN) ),
	convert(LengthN, LengthUnits, LengthC, WidthUnits),
	Area is LengthC * WidthN,
	area_units_for_length(WidthUnits, AreaUnits),
	store_statement(Subject, Predicate, literal(type(AreaUnits, Area)), gu:'Geometry', ObjectNode).

s_fetch(Subject, Predicate, ObjectNode, user) :-
	\+rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvveoTpwpEbGdrcN5Y29ycA'), % surfaceAreaOfWholeObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	!,
	fetch(Subject, ocyc:'Mx4rvVjbZpwpEbGdrcN5Y29ycA', LengthNode, _), % lengthOfObject
	fetch(Subject, ocyc:'Mx4rvVjgA5wpEbGdrcN5Y29ycA', WidthNode, _), % widthOfObject
	fetch(Subject, ocyc:'Mx4rvViZ85wpEbGdrcN5Y29ycA', DepthNode, _), % depthOfObject
	%% Extract length and width for object
	rdf(LengthNode, rdf:value, literal(type(xsd:float, Length))),
	rdf(LengthNode, gu:units, LengthUnits),
	rdf(WidthNode, rdf:value, literal(type(xsd:float, Width))),
	rdf(WidthNode, gu:units, WidthUnits),
	rdf(DepthNode, rdf:value, literal(type(xsd:float, Depth))),
	rdf(DepthNode, gu:units, DepthUnits),
	!,
	%% Make sure we have numbers
	( number(Length) -> LengthN = Length ; atom_number(Length, LengthN) ),
	( number(Width) -> WidthN = Width ; atom_number(Width, WidthN) ),
	( number(Depth) -> DepthN = Depth ; atom_number(Depth, DepthN) ),
	convert(LengthN, LengthUnits, LengthC, WidthUnits),
	convert(DepthN, DepthUnits, DepthC, WidthUnits),
	Area is 2 * LengthC * WidthN + 2 * DepthC * WidthN + 2 * DepthC + LengthC,
	area_units_for_length(WidthUnits, AreaUnits),
	store_statement(Subject, Predicate, literal(type(AreaUnits, Area)), gu:'Geometry', ObjectNode).

s_fetch(Subject, Predicate, ObjectNode, user) :-
	\+rdfs_individual_of(Subject, ocyc:'Mx4rwQBfkZwpEbGdrcN5Y29ycA'), % RoundObject
	rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVjbrZwpEbGdrcN5Y29ycA'), % volumeOfObject
	rdfs_individual_of(Subject, ocyc:'Mx4r1jUXeq00EdmAAAACs0uFOQ'), % ThreeDimensionalThing
	!,
	fetch(Subject, ocyc:'Mx4rvVjbZpwpEbGdrcN5Y29ycA', LengthNode, _), % lengthOfObject
	fetch(Subject, ocyc:'Mx4rvVjgA5wpEbGdrcN5Y29ycA', WidthNode, _), % widthOfObject
	fetch(Subject, ocyc:'Mx4rvViZ85wpEbGdrcN5Y29ycA', DepthNode, _), % depthOfObject
	%% Extract length and width for object
	rdf(LengthNode, rdf:value, literal(type(xsd:float, Length))),
	rdf(LengthNode, gu:units, LengthUnits),
	rdf(WidthNode, rdf:value, literal(type(xsd:float, Width))),
	rdf(WidthNode, gu:units, WidthUnits),
	rdf(DepthNode, rdf:value, literal(type(xsd:float, Depth))),
	rdf(DepthNode, gu:units, DepthUnits),
	!,
	%% Make sure we have numbers
	( number(Length) -> LengthN = Length ; atom_number(Length, LengthN) ),
	( number(Width) -> WidthN = Width ; atom_number(Width, WidthN) ),
	( number(Depth) -> DepthN = Depth ; atom_number(Depth, DepthN) ),
	convert(LengthN, LengthUnits, LengthC, WidthUnits),
	convert(DepthN, DepthUnits, DepthC, WidthUnits),
	Volume is LengthC * WidthN * DepthC,
	volume_units_for_length(WidthUnits, VolumeUnits),
	store_statement(Subject, Predicate, literal(type(VolumeUnits, Volume)), gu:'Geometry', ObjectNode).

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