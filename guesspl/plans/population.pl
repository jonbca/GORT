%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for calculating populations, and population-related
%% guesstimation.
:- include('meta/operators'),
   ensure_loaded('meta/interpreter').
   
:- multifile noprove/1.

noprove(sum_list).
noprove(population_of_region). % too big!
%% Goal to locate the regions of a given type. For example,
%  subregion('NorthAmericanCountry', Subregions) will unify
%  Subregions with a list of countries in North America.
subregions(RegionURI, Subregions) :-
    not( is_region(RegionURI) ),
    findall(SubregionURI, rdfs:rdfs_individual_of(SubregionURI, RegionURI),
    	Subregions).

%% Goal to locate the subregions of a particular region. For
%  example, subregion('ContinentOfNorthAmerica', Subregions) will unify
%  Subregions with a list of cities, towns, US states, countries, etc.
%  located on the continent of North America.
subregions(RegionURI, Subregions) :-
    is_region(RegionURI),
    findall(SubregionURI, subregion(RegionURI, SubregionURI), Subregions).

%% Goal unifies Superregion with a broader term for Subregion. Since
%  broaderTerms encompass many different Cyc concepts, this goal succeeds
%  only if both Superregion and Subregion are both geographicalRegions,
%  as defined in the is_region/1 goal.

%%% FIXME alter this to support wordnet mereonyms
subregion(Superregion, Subregion) :-
    cyclify(BroaderTerm, 'broaderTerm'),
    rdf(Subregion, BroaderTerm, Superregion),
    is_region(Superregion),
    is_region(Subregion).

%% Goal succeeds if RegionURI is a Geographical Region
is_region(RegionURI) :-
    cyclify(GeoRegion, 'GeographicalRegion'),
    rdfs_individual_of(RegionURI, GeoRegion).

%% Goals to find the population of a region. The first goal
%  attempts to directly solve for the population of a region by
%  checking for a numberOfInhabitants property or subproperty.
%  The second goal attempts to find the population by summing the
%  individual populations of the subregions of the region given in
%  RegionURI. In both cases, Population unifies with an order-of-magnitude
%  estimate.
find_population_of_region( RegionURI, Population ) :-
    cyclify(InhabitantsURI, 'numberOfInhabitants'),
    solve(RegionURI, InhabitantsURI, [literal(type(_, PopVal)) | _]),
    to_om(PopVal, Population).

find_population_of_region( RegionURI, Population ) :-
    subregions(RegionURI, Subregions),
    population_of_region(Subregions, PopList),
    extract_pop_data(PopList, PopNList),
    sum_list(PopNList, PopVal),
    to_om(PopVal, Population).

noprove(extract_pop_data).
extract_pop_data( [], [] ).
extract_pop_data( [region_pop(_, PopN) | PopListTail], [PopN | PopTail] ) :-
    extract_pop_data( PopListTail, PopTail).

%% Locate the populations of a list of regions by attempting to solve
%  for the numberOfInhabitants property for each region. In the case
%  that the information is unavailable, use 0 for the region's population.
%  Issue a warning.

population_of_region( [], [] ).
population_of_region( [RegionURI | RegionTail], [region_pop(RegionURI, Population) | PopTail] ) :-
    population(RegionURI, Population),
    population_of_region(RegionTail, PopTail).

population_of_region( [RegionURI | RegionTail], Pop ) :-
    \+population(RegionURI, _),
    population_of_region(RegionTail, Pop).

population(RegionURI, Population) :-
    solve(RegionURI,
          'http://sw.opencyc.org/concept/Mx4rvVjy1pwpEbGdrcN5Y29ycA', %numberOfInhabitants
          [literal(type(_, Population)) | _]).

%% Utility goals to sum a list of numbers (Bratko 2001)
sum_list( List, Sum ) :-
    sum_list_tail(List, 0, Sum).

sum_list_tail([], Sum, Sum).

sum_list_tail([H | T], PartialSum, Sum) :-
    atom(H),
    atom_number(H, N),
    NewPartialSum is PartialSum + N,
    sum_list_tail(T, NewPartialSum, Sum).
    
sum_list_tail([H | T], PartialSum, Sum) :-
    number(H),
    NewPartialSum is PartialSum + H,
    sum_list_tail(T, NewPartialSum, Sum).