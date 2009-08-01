%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for inferring part-whole relations using
%% the WordNet databases.

noprove(sum_list).

%% Update the OpenCyc -> WordNet associations to point to resources instead of
%% literals.
fix_ocyc_synsets :-
    findall( uripair(CycURI, Synset),
             rdf(CycURI, 'http://sw.opencyc.org/concept/Mx4riWVFR6HJSpaEaHrcWS3MSA',
             literal(Synset)), URIPairs ),
   	rdf_transaction( fix_ocyc_synsets(URIPairs) ).

fix_ocyc_synsets([]).

fix_ocyc_synsets([uripair(CycURI, Synset) | URIs]) :-
	fix_ocyc_synsets(URIs),
	rdf_update(CycURI,
		'http://sw.opencyc.org/concept/Mx4riWVFR6HJSpaEaHrcWS3MSA',
     	literal(Synset),
     	object(Synset)).

cyc_wordnet_uri(CycURI, WordnetURI) :-
    rdf(CycURI, 'http://sw.opencyc.org/concept/Mx4riWVFR6HJSpaEaHrcWS3MSA', WordnetURI).

/** wn_part_of(?Part, ?Node) is multi.

True if the resource identified by the URI in Part is engaged in a 
wns:partMeronymOf relation with Node.

@param Part the part in the merological relation
@param Node the whole in the merological relation
*/
wn_part_of(Part, Node) :-
    rdf(Part, wns:partMeronymOf, Node).
    
%wn_part_of(Part, Node) :-
%    rdf(Node, 'http://sw.opencyc.org/concept/Mx4riWVFR6HJSpaEaHrcWS3MSA', WordnetNode),
%    rdf(Part, 'http://sw.opencyc.org/concept/Mx4riWVFR6HJSpaEaHrcWS3MSA', WordnetPart),
%    rdf(WordnetPart, wns:partMeronymOf, WordnetNode).

/** parts(+Node, -Parts) is det.
True if Parts is a list of the parts of Node, according to Wordnet.

@param Node the Whole in the part-whole relation
@param Parts the parts in the part-whole relation
*/
parts(Node, Parts) :-
    findall( Part, wn_part_of(Part, Node), Parts).

/** aggregate_parts(+StartNodeURI, +PropertyURI, -Aggregate) is det.
Obtains all of the parts of StartNodeURI and aggregates the values related
by the PropertyURI relation, or a subproperty thereof. Places the oom
result in Aggregate and records the aggregation in the customised ontology.

*/
aggregate_parts( StartNodeURI, PropertyURI, AggregateN ) :-
    cyc_wordnet_uri(StartNodeURI, StartNodeWordnetURI),
    aggregate_parts(StartNodeWordnetURI, PropertyURI, AggregateN).
    
aggregate_parts(StartNodeWordnetURI, PropertyURI, AggregateN) :-
    parts(StartNodeWordnetURI, Parts),
    part_values(Parts, PropertyURI, ValueList),
    sum_list(ValueList, AggregateN),
    AggregateN =\= 0.

part_values([], _, []).

part_values([PartURI | PartTail], PropertyURI, [Value | ValueTail]) :-
    part_values(PartTail, PropertyURI, ValueTail),
    (
    	\+fetch(PartURI, PropertyURI, _, user),
    	fetch(PartURI, PropertyURI, ObjectNode, _),
    	rdf(ObjectNode, rdf:value, literal(type(xsd:float, Value)))
    		-> true
    		; Value = 0
    ).
