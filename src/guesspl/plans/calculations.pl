%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains rewrite rules for calculating volumes, areas, lengths, etc.

:- ensure_loaded('conversion').

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
area_units_for_length(LengthUnit, AreaUnit) :-
    atom_concat('sq_', LengthUnit, AreaUnit).
    
volume_units_for_length(LengthUnit, VolumeUnit) :-
	atom_concat('cu_', LengthUnit, VolumeUnit).