do_pickle(Result) :-
	cyclify(Pickle, 'Pickle'),
	cyclify(Len, 'lengthOfObject'),
	fetch(Pickle, Len, PickleLength, _),
	cyclify(Canada, 'Canada'),
	cyclify(Inh, 'numberOfInhabitants'),
	fetch(Canada, Inh, CanadaInh, _),
	once((rdf(PickleLength, rdf:value, literal(type(xsd:float, LPickle))),
	rdf(PickleLength, gu:units, PickleUnits),
	rdf(CanadaInh, rdf:value, literal(type(xsd:float, NCanadaInh))))),
	multiply(units(literal(type(xsd:float, NCanadaInh)), Canada),
		units(literal(type(xsd:float, LPickle)), PickleUnits), Result).