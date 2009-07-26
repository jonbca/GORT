%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file manages the fact retrieval mechanism

:- use_module(library(readutil)).

:- ensure_loaded('plans/epsilon').

:- rdf_meta fetch(r, r, t, -), store_statement(r, r, t, r, r).

abolish_customised_ontology :-
    rdf_retractall(_,_,_,user).

store_statement(Subject, Predicate, literal(type(Type, Value)), Graph, ObjectNode) :-
    rdf_transaction((
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(xsd:float, Value))),
	    to_om(Value, ValueOM),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).

/** fetch(+Subject, +Predicate, -Object, -Graph) is nondet.

Fetches a triple from the store, and if necessary, copies it to the 
customised ontology (named graph 'user') and puts in a reifying
statement describing the source of the data. The Object unifies
with a URI for a blank node that contains an rdf:value relation to the
actual value of the returned object, and a gu:units relation to the
actual unit of measure for the returned object. The object's rdf:value
relation is to a literal, typically of type gu:oom. The literal may hold
any datatype, however. The object of the gu:units predicate is the
unit of measure for the value in the rdf:value predicate.

@param Subject the Subject of the triple being sought
@param Predicate the Predicate of the triple being sought
@param Object the Object of the predicate being sought
@param Graph the named graph containing the original triple source
*/
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    rdf(Subject, Predicate, ObjectNode, user), !.
    
%%%%%%%%%%%%%% General purpose plans %%%%%%%%%%%%%%%
% Fetch epsilon value for a Class
fetch(Class, Predicate, ObjectNode, user) :-
    \+rdf(Class, Predicate, ObjectNode, user),
    epsilon_exists(Class),
    rdf(TargetSubject, rdf:type, gu:'Epsilon'),
    rdf(TargetSubject, rdf:type, Class),
    rdf(TargetSubject, Predicate, ObjectNode).

% Fetch direct value for non-class, that has not already been stored in
% the user ontology
fetch(Subject, Predicate, ObjectNode, Graph) :-
    \+rdf(Subject, Predicate, _Object, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    rdf(Subject, Predicate, literal(type(Type, ObjectN)), Graph:_GraphID),
    store_statement(Subject, Predicate, literal(type(Type, ObjectN)), Graph, ObjectNode).

% Fetch indirect value for non-class that has not already been stored in
% the user ontology
fetch(Subject, Predicate, ObjectNode, Graph) :-
    \+rdf(Subject, Predicate, _Object, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    solve(Subject, Predicate, [literal(type(Type, ObjectN))| _Path], Graph:_GraphID),
    store_statement(Subject, Predicate, literal(type(Type, ObjectN)), Graph, ObjectNode).

% As above, for WordNet Synsets, which are declared as classes
fetch(Subject, Predicate, ObjectNode, Graph) :-
    \+rdf(Subject, Predicate, _Object, user),
    rdfs_individual_of(Subject, wns:'Synset'),
    solve(Subject, Predicate, [literal(type(Type, ObjectN))|_Path], Graph:_GraphID),
    store_statement(Subject, Predicate, literal(type(Type, ObjectN)), Graph, ObjectNode).    

%%%%%%%%%%%%%% Special purpose plans %%%%%%%%%%%%%%%

%% Special plan for population
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _Object, user),
    is_region(Subject),
    rdfs_subproperty_of(Predicate, ocyc:'Mx4rvVjy1pwpEbGdrcN5Y29ycA'),
   	rdf_transaction(( find_population_of_region(Subject, PopN, _),
    store_statement(Subject, Predicate, literal(type(xsd:long, PopN)), gu:'Function', ObjectNode))).
    
%%%%%%%%%%%%%% Aggregation plans %%%%%%%%%%%%%%%
% Do aggregation for a decomposable object (according to WordNet's partOf relations)
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    aggregate_parts(Subject, Predicate, Aggregate),
    store_statement(Subject, Predicate, literal(type(xsd:long, Aggregate)), gu:'Aggregation', ObjectNode).

% Do aggregation for a decomposable object, which is a Synset
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _, user),
    rdfs_individual_of(Subject, wns:'Synset'),
    aggregate_parts(Subject, Predicate, Aggregate),
    store_statement(Subject, Predicate, literal(type(xsd:long, Aggregate)), gu:'Aggregation', ObjectNode).

%%%%%%%%%%%%%% Epsilon plans %%%%%%%%%%%%%%%
% Compute epsilon value for a class
fetch(Class, Predicate, ObjectNode, user) :-
    \+epsilon_exists(Class),
    rdfs_individual_of(Class, rdfs:'Class'),
    get_average_value(Class, Predicate, type(Type, Average)),
    symbol_uri(Type, TypeURI),
    rdf_transaction((
    	rdf_node(EpsilonNode),
    	rdf_assert(EpsilonNode, rdf:type, Class),
    	rdf_assert(EpsilonNode, rdf:type, gu:'Epsilon')
    )),
    store_statement(EpsilonNode, Predicate, literal(type(TypeURI, Average)), gu:'System', ObjectNode).

%%%%%%%%%%%%%% User Interaction Plans %%%%%%%%%%%%%%
% Ask the user for an epsilon value for a class, but not a Synset
fetch(Class, Predicate, ObjectNode, user) :-
    \+epsilon_exists(Class),
    \+rdfs_individual_of(Class, wns:'Synset'),
    rdfs_individual_of(Class, rdfs:'Class'),
    \+get_average_value(Class, Predicate, _),
    ask_user(Class, Predicate, type(Type, N)),
    rdf_transaction((
    	rdf_node(EpsilonNode),
    	rdf_assert(EpsilonNode, rdf:type, Class),
    	rdf_assert(EpsilonNode, rdf:type, gu:'Epsilon')
    )),
    store_statement(EpsilonNode, Predicate, literal(type(Type, N)), gu:'CurrentUser', ObjectNode).

% Ask the user for a value for an object
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    ask_user(Subject, Predicate, type(Type, N)),
    store_statement(Subject, Predicate, literal(type(Type, N)), gu:'CurrentUser', ObjectNode).

%%%%%%%%%%%%%% End of Fetch plans %%%%%%%%%%%%%%%

/** epsilon_exists(?Class) is nondet.
Goal succeeds if Class refers to an RDFS class that already
has an epsilon node in the custom ontology.
*/
epsilon_exists(Class) :-
    rdfs_individual_of(Class, rdfs:'Class'),
    rdfs_individual_of(T, Class),
    rdfs_individual_of(T, gu:'Epsilon').

ask_user(Class, Property, type(Type, Value)) :-
    (cyclify(Class, EnglishClass) -> true ; EnglishClass = Class ),
    (cyclify(Property, EnglishProperty) -> true ; EnglishProperty = Property),
    prompt(_, 'ask> '),
    write('enter value for: '),
    writeln(rdf(EnglishClass, EnglishProperty, '?')),
    read_input_line(Tokens),
    parse_input(Tokens, Type, Value).
    
read_input_line(Tokens) :-
    read_line_to_codes(user_input, Codes),
    atom_codes(String, Codes),
    concat_atom(Tokens, ' ', String).
    
parse_input([Value, Symbol], Type, Value) :-
    symbol_uri(Symbol, Type), !.
    
parse_input([Value, '-', Value2, Symbol], Type, ValueOut) :-
    symbol_uri(Symbol, Type), !,
    geomean([Value, Value2], ValueOut).