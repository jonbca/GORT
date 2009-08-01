%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for calculating figures for geographical regions.
:- use_module(library('semweb/rdf_db')).

:- ensure_loaded('../meta/operators'),
   ensure_loaded('../meta/interpreter'),
   ensure_loaded('../plans/fetch'),
   ensure_loaded('../plans/wordnet_meronymy'),
   ensure_loaded('../declarations').

:- rdf_meta find_value_for_region(r, r, -, -).

noprove(find_value_for_region). % too big!
noprove(extract_figure_data).
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

find_value_for_region(RegionURI, Property, Value, _) :-
    solve(RegionURI, Property, [literal(type(_, Value)) | _], _Graph).

find_value_for_region(RegionURI, Property, Value, _) :-
	\+solve(RegionURI, Property, _, _),
	subregions(RegionURI, Subregions, RegionType),
	cyclify(RegionType, RegionTypeEn),
	info('Processing region type '), info(RegionTypeEn),
	figure_for_region(Subregions, Property, FigList),
	extract_figure_data(FigList, FigNList),
	sum_list(FigNList, Value),
	Value =\= 0.

extract_figure_data( [], [] ).
extract_figure_data( [region_pop(_, FigureN) | FigListTail], [FigureN | FigTail] ) :-
    extract_figure_data( FigListTail, FigTail).

%% Locate the populations of a list of regions by attempting to solve
%  for the numberOfInhabitants property for each region. In the case
%  that the information is unavailable, use 0 for the region's population.
%  Issue a warning.
figure_for_region( [], _, [] ).
figure_for_region( [RegionURI | RegionTail], Property, [region_pop(RegionURI, Figure) | FigTail] ) :-
    figure(RegionURI, Property, Figure),
    figure_for_region(RegionTail, Property, FigTail).

figure_for_region( [RegionURI | RegionTail], Property, Fig ) :-
    \+figure(RegionURI, Property, _),
    cyclify(RegionURI, RegionEn),
    info('Missing figure for region '), info(RegionEn),
    figure_for_region(RegionTail, Property, Fig).

figure(RegionURI, Property, Figure) :-
    solve(RegionURI,
          Property,
          [literal(type(_, Figure)) | _]), !.
          
    
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