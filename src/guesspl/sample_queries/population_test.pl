%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains tests for calculating populations, and population-related
%% guesstimation.

population_north_america(Population) :-
    cyclify(NorthAmericaURI, 'ContinentOfNorthAmerica'),
    find_population_of_region(NorthAmericaURI, Population).
    
population_latin_america(Population) :-
    cyclify(NorthAmericaURI, 'LatinAmericanCountry'),
    find_population_of_region(NorthAmericaURI, Population).
    
population_europe(Population) :-
    cyclify(NorthAmericaURI, 'ContinentOfEurope'),
    find_population_of_region(NorthAmericaURI, Population).
    
population_europe(Population) :-
    cyclify(NorthAmericaURI, 'EuropeanCountry'),
    find_population_of_region(NorthAmericaURI, Population).
    
population_earth(Population) :-
    cyclify(Earth, 'PlanetEarth'),
    find_population_of_region(Earth, Population).