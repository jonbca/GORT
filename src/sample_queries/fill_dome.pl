%Need dome volume
%Need flow rate of water (ie per second)

flow(X) :-
	cyclify(V, volumeOfObject),
	cyclify(F, flowCapacity),
	cyclify(Tap, 'Tap-PlumbingFixture'),
	fetch('http://dbpedia.org/resource/St_Paul''s_Cathedral', V, VolObject, _),
	fetch(Tap, F, FlowObject, _),
	value_for_object(VolObject, Volume),
	value_for_object(FlowObject, FlowRate),
	divide(Volume, FlowRate, X).

value_for_object(Object, units(Value, Units)) :-
	rdf(Object, rdf:value, Value),
	rdf(Object, gu:units, Units),!.