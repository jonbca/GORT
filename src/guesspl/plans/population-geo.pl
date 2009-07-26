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
   ensure_loaded('meta/interpreter'),
   ensure_loaded('plans/fetch'),
   ensure_loaded('plans/wordnet_meronymy').

noprove(population_of_region). % too big!
noprove(extract_pop_data).
noprove(sum_list).

%% Goal to locate the regions of a given type. For example,
%  subregion('NorthAmericanCountry', Subregions) will unify
%  Subregions with a list of countries in North America.
subregions(RegionURI, Subregions, _) :-
    not( is_region(RegionURI) ),
    findall(SubregionURI, rdfs:rdfs_individual_of(SubregionURI, RegionURI),
    	Subregions).

%% Goal unifies Superregion with a broader term for Subregion. Since
%  broaderTerms encompass many different Cyc concepts, this goal succeeds
%  only if both Superregion and Subregion are both geographicalRegions,
%  as defined in the is_region/1 goal.
subregions(RegionURI, Subregions, RegionType) :-
    is_region(RegionURI),
    % Region types include City, Borough, County, Province, State, etc.
    rdf(RegionType, rdf:type, ocyc:'Mx4rv-FNbpwpEbGdrcN5Y29ycA'),
    rdfs_individual_of(RegionType, ocyc:'Mx4rPJJhlM0SQdeDhsNnnhQekw'),
    findall(SubregionURI,
            subregion_star(RegionURI, subregion_by_type(SubregionURI, RegionType)),
            SubregionsList),
    list_to_set(SubregionsList, Subregions).

subregion_star(SuperregionURI, subregion_by_type(SubregionURI, ClassURI)) :-
    % get broader terms for super-region
    rdf(SubregionURI,
      'http://sw.opencyc.org/concept/Mx4rZOAVeiYGEdqAAAACs2IMmw',
      SuperregionURI),
    % get the classes that the candidate subregion is an instance of
    rdfs_individual_of(SubregionURI, ClassURI).
    
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

find_population_of_region( RegionURI, PopVal, _ ) :-
    cyclify(InhabitantsURI, 'numberOfInhabitants'),
    solve(RegionURI, InhabitantsURI, [literal(type(_, PopVal)) | _], _Graph).

find_population_of_region( RegionURI, PopVal, RegionType ) :-
    cyclify(InhabitantsURI, 'numberOfInhabitants'),
    \+solve(RegionURI, InhabitantsURI, [literal(type(_, PopVal)) | _], _),
    subregions(RegionURI, Subregions, RegionType),
    cyclify(RegionType, RegionTypeEn),
    info('Processing region type '), info(RegionTypeEn),
    population_of_region(Subregions, PopList),
    extract_pop_data(PopList, PopNList),
    sum_list(PopNList, PopVal),
    PopVal =\= 0.

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
    cyclify(RegionURI, RegionEn),
    info('Missing population for region '), info(RegionEn),
    population_of_region(RegionTail, Pop).

population(RegionURI, Population) :-
    solve(RegionURI,
          'http://sw.opencyc.org/concept/Mx4rvVjy1pwpEbGdrcN5Y29ycA', %numberOfInhabitants
          [literal(type(_, Population)) | _]), !.