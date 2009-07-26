ui_population(Region) :-
    cyclify(RegionURI, Region),
    find_population_of_region(RegionURI, Population),
    print_msg('Population: ', [Region, ' is ', Population]),
    fail ; true.