%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% Evaluation plans

:- rdf_load('../guessdata/dbpedia_extracts/persons.ttl').
:- ensure_loaded('../plans/division_plan').
:- ensure_loaded('../plans/size_plan').
:- rdf_load('../guessdata/dbpedia_extracts/Loch_Ness.rdf').

volume_cell(TotalVolume) :-
	cyclify(Cell, 'BloodCell'),
	cyclify(VolConcept, 'volumeOfObject'),
	fetch(Cell, VolConcept, TotalVolume, _).
	
volume_human_body(TotalVolume) :-
	cyclify(VolConcept, 'volumeOfObject'),
	fetch(dbo:'Person', VolConcept, TotalVolume, _).

%%%%%   EVALUATION PLAN 1 - HOW MANY CELLS IN HUMAN BODY?
qty_cells_human_body(TotalCount) :-
	cyclify(VolConcept, 'volumeOfObject'),
	cyclify(Cell, 'BloodCell'),
	quantity_of(Cell, VolConcept, dbo:'Person', VolConcept, TotalCount),!.
	
%%%%%   EVALUATION PLAN 2 - VOLUME OF GOLF BALLS IN LOCH NESS?
qty_balls_loch_ness(TotalCount) :-
	cyclify(VolConcept, 'volumeOfObject'),
    cyclify(GolfBall, 'GolfBall'),
    quantity_of(GolfBall, VolConcept, dbpedia:'Loch_Ness', VolConcept, TotalCount),!.

%%%%%   EVALUATION PLAN 3 - TOTAL VOLUME OF BLOOD ON EARTH?
qty_blood_earth(TotalVolume) :-
	cyclify(VolConcept, 'volumeOfObject'),
	cyclify(Cell, 'BloodCell'),
	cyclify(Earth, 'PlanetEarth'),
	cyclify(Inhabitants, 'numberOfInhabitants'),
	qty_cells_human_body(CellCount),!,
	total_size_s(CellCount, gu:result, Earth, Inhabitants, TotalCellCount),!,
	total_size_s(Cell, VolConcept, TotalCellCount, gu:result, TotalVolume),!.
	