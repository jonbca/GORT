%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains rewrite rules for converting units.

:- ensure_loaded('../meta/cycutils').

:- multifile noprove/1.
:- dynamic noprove/1.

:- rdf_register_ns(gu, 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/').
:- rdf_load('../../guessdata/operator_mappings.ttl').

noprove(same_dimension). % do not prove because of verbosity of proof
noprove(symbol_uri).   % do not prove because of cut
noprove(scale_factor). % do not prove because of cut
noprove(split_input).  % do not prove split input -- trivial

:- op(400, xf, cnt).   % cnt is "count", a suffix denoting a dimensionless quantity

% Map the symbols given in the RDF file to Prolog operators, which represent
% the unit of measure concepts in the RDF database.
map_operators :-
    rdf(_, gu:prologSymbol, literal(Symbol)),
    op(400, xf, Symbol), fail ; true.

%% convert(+From, ?To) is semidet.
%
%  Converts a measurement. The To argument should be specified as a variable and a unit or ratio of units,
%  for example:
%  ==
%  convert(10 km, Z m)
%  convert(1 mi/gal, Y km/l)
%  convert(1 mi/gal, Z l/km)
%  ==
%  will convert 10 km to metres, and place the result in A.
%
%  @param From the measurement to convert, specified with an appropriate operator (e.g. '10 km')
%  @param To the result. Units for the result should be specified, and a variable placeholder used for the numeric component.   
convert(From, To) :-
    split_input(From, FromN, FromS),
    split_input(To, ToN, ToS),
    convert(FromN, FromS, ToN, ToS).

convert(From, To) :-
    From =.. [/, FromTopMeasure, FromBottomUnits],
    split_input(FromTopMeasure, FromN, FromTopUnits),
    To =.. [/, ToTopMeasure, ToBottomUnits],
    split_input(ToTopMeasure, ToN, ToTopUnits),
    convert(FromN, FromTopUnits, ToTopN, ToTopUnits),
    convert(1, FromBottomUnits, ToBottomN, ToBottomUnits),
    ToN is ToTopN / ToBottomN.

% Handles inverted units e.g. mi/hr -> sec/m
convert(From, To) :-
    From =.. [/, FromTopMeasure, FromBottomUnits],
    split_input(FromTopMeasure, FromN, FromTopUnits),
    To =.. [/, ToTopMeasure, ToBottomUnits],
    split_input(ToTopMeasure, ToN, ToTopUnits),
    convert(FromN, FromTopUnits, ToTopN, ToBottomUnits),
    convert(1, FromBottomUnits, ToBottomN, ToTopUnits),
    ToN is ToBottomN / ToTopN.

convert(FromN, FromS, ToN, ToS) :-
    number(FromN),
    symbol_uri(FromS, FromSURI),
    symbol_uri(ToS, ToSURI),
    same_dimension(FromSURI, ToSURI), !,   %Red cut
    scale_factor(FromSURI, FromFactor),
    scale_factor(ToSURI, ToFactor),
    ToN is FromN * FromFactor / ToFactor.

convert(FromAtom, FromS, ToN, ToS) :-
    catch( atom_number(FromAtom, FromN), _, fail),
    symbol_uri(FromS, FromSURI),
    symbol_uri(ToS, ToSURI),
    same_dimension(FromSURI, ToSURI), !,   %Red cut
    scale_factor(FromSURI, FromFactor),
    scale_factor(ToSURI, ToFactor),
    ToN is FromN * FromFactor / ToFactor.

%% split_input(+Measure, -Number, -Symbol) is det.
% Takes a Measure from the user (of the form "N S", where N is a number
% and S is a symbol for an operator defined 
% in operator_mappings.ttl.
%
% @param Measure The fully specified measurement (e.g. '10 km')
% @param Number The numeric component of the measurement (e.g. 10)
% @param Symbol The symbolic component of the measurement representing the dimension of the quantity (e.g. km)
split_input(Measure, Number, Symbol) :-
    Measure =.. [Symbol, Number].

%% symbol_uri(?Unit, ?UnitURI) is det.
%
%  True if the operator (e.g. 'km') for the dimension is the same as the
%  concept represented by the URI in UnitURI in the RDF store.
%
%  @param Unit The unit operator for a particular measurement concept (e.g. 'km')
%  @param UnitURI The URI that represents the measurement concept in the RDF store
symbol_uri(Unit, UnitURI) :-
    rdf(UnitURI, gu:prologSymbol, literal(Unit)).
    
symbol_uri(UnitURI, CycURI) :-
    solve(UnitURI, gu:prologSymbol, [literal(_), CycURI |_]).

%% scale_factor(+UnitURI, -Factor) is det.
%
%  Unifies Factor with a relative numeric conversion factor for
%  the given UnitURI.
%
%  @param UnitURI The URI for the unit to find the conversion factor for
%  @param Factor  A floating-point number that relates the given unit to some base unit.
scale_factor(UnitURI, Factor) :-
    rdf(UnitURI, gu:scaleFactor, literal(type(_,FactorA))),
    !,								% Only care about 1 result, so cut
    atom_number(FactorA, Factor).

%% same_dimension( +UnitAURI, +UnitBURI ) is semidet.
%
%  True if UnitA and UnitB are of the same dimension type,
%  i.e. UnitA is for a distance and UnitB is for a distance. The unit
%  types should be given as URIs.
%
%  @param UnitA The first unit to compare.
%  @param UnitB The second unit to compare.
same_dimension('http://sw.opencyc.org/concept/Mx4rvVi_aZwpEbGdrcN5Y29ycA', %dimensionless
               'http://sw.opencyc.org/concept/Mx4rvVi_aZwpEbGdrcN5Y29ycA') :- !. %cut on one result
same_dimension(UnitAURI, UnitBURI) :-
    \+var(UnitAURI),
    \+var(UnitBURI),
    rdf(UnitAURI, rdf:type, UnitAClass),
    ( 
    	rdfs_subclass_of(UnitAClass, ocyc:'Mx4ruFr2Fp-7QdiS0NRO_LP5CA'), ! % One-dimensional unit of measure
    ;
    	rdfs_subclass_of(UnitAClass, ocyc:'Mx4rVG-MSJ_AQdiKntlmhkDKfQ') % Multi-dimensional unit of measure
    ),
    rdf(UnitBURI, rdf:type, UnitAClass).
    
length_unit(Measure) :-
    Measure =.. [Symbol, _],
    symbol_uri(Symbol, SymbolURI),
    %% UnitOfDistance
    rdfs_individual_of(SymbolURI, ocyc:'Mx4rvViKt5wpEbGdrcN5Y29ycA').
    		
volume_unit(Measure) :-
    Measure =.. [Symbol, _],
    symbol_uri(Symbol, SymbolURI),
    %% UnitOfVolume
    rdfs_individual_of(SymbolURI, ocyc:'Mx4rvVj9n5wpEbGdrcN5Y29ycA').
    
area_unit(Measure) :-
    Measure =.. [Symbol, _],
    symbol_uri(Symbol, SymbolURI),
    %% UnitOfVolume
    rdfs_individual_of(SymbolURI, ocyc:'Mx4rvVjCx5wpEbGdrcN5Y29ycA').
    
mass_unit(Measure) :-
    Measure =.. [Symbol, _],
    symbol_uri(Symbol, SymbolURI),
    %% UnitOfVolume
    rdfs_individual_of(SymbolURI, ocyc:'Mx4rvViKcpwpEbGdrcN5Y29ycA').
    
time_unit(Measure) :-
    Measure =.. [Symbol, _],
    symbol_uri(Symbol, SymbolURI),
    %% UnitOfVolume
    rdfs_individual_of(SymbolURI, ocyc:'Mx4rvVj5xZwpEbGdrcN5Y29ycA').

dimensionless_unit(cnt(_)).