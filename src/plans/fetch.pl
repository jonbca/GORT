%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file manages the fact retrieval mechanism

:- use_module(library(readutil)).
:- use_module(library('semweb/rdf_db')).
:- ensure_loaded('epsilon').
:- ensure_loaded('../declarations').
:- ensure_loaded('conversion').

:- rdf_meta fetch(r, r, t, -), store_statement(r, r, t, r, r).

abolish_customised_ontology :-
    rdf_retractall(_,_,_,user).

/** store_statement(+Subject, +Predicate, ?Object, +Graph, -ObjectNode) is det.
Records a statement in the customised ontology, and adds its reification.
*/
store_statement(Subject, Predicate, literal(type(Type, Value)), Graph, ObjectNode) :-
	\+var(Type),
	once(rdf(Subject, Predicate, _)),
    rdf_transaction((
    	rdf_retractall(Subject, Predicate, _),
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(xsd:float, Value))),
	    to_om(Value, ValueOM),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).

store_statement(Subject, Predicate, literal(type(Type, Value)), Graph, ObjectNode) :-
	var(Type),
	once(rdf(Subject, Predicate, _)),
	fix_units(Subject, Predicate, Type, Value),
    rdf_transaction((
    	rdf_retractall(Subject, Predicate, _),
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(xsd:float, Value))),
	    to_om(Value, ValueOM),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).


store_statement(Subject, Predicate, literal(type(Type, Value)), Graph, ObjectNode) :-
	\+var(Type),
	\+rdf(Subject, Predicate, _),
	Value \= om(_,_),
    rdf_transaction((
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(xsd:float, Value))),
	    to_om(Value, ValueOM),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).

store_statement(Subject, Predicate, literal(type(Type, om(M,E))), Graph, ObjectNode) :-
	\+var(Type),
	\+rdf(Subject, Predicate, _),
    rdf_transaction((
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, om(M,E)))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).

% Records a statement if the units are not known. Asks the user to specify the units.
store_statement(Subject, Predicate, literal(type(Type, Value)), Graph, ObjectNode) :-
	var(Type),
	\+rdf(Subject, Predicate, _),
	fix_units(Subject, Predicate, Type, Value),
    rdf_transaction((
	    rdf_node(ObjectNode),
	    rdf_assert(Subject, Predicate, ObjectNode),
	    rdf_assert(ObjectNode, rdf:value, literal(type(xsd:float, Value))),
	    to_om(Value, ValueOM),
	    rdf_assert(ObjectNode, rdf:value, literal(type(gu:oom, ValueOM))),
	    rdf_assert(ObjectNode, gu:units, Type),
	    reify_create(rdf(Subject, Predicate, ObjectNode), dc:source, Graph, _)
    )).




/** fix_units(+Subject, +Property, -Type, +Value) is det.

Prompt the user for missing units on a measurement from the ontology.
*/
fix_units(Subject, Property, Type, Value) :-
    (cyclify(Subject, EnglishSubject) -> true ; EnglishSubject = Subject ),
    (cyclify(Property, EnglishProperty) -> true ; EnglishProperty = Property),
	writeln('No units found for: '),
	writeln(rdf(EnglishSubject, EnglishProperty, literal(Value))),
	prompt(_, 'units> '),
	writeln('Enter appropriate units.'),
	read_input_line([Units]),
	symbol_uri(Units, Type), !.

fix_dbpunits(Subject, Predicate, Measure) :-
	rdf(Subject, Predicate, literal(lang(en, Value))),
	atom_chars(Value, Chars),
	delete(Chars, ',', NewChars),
	atom_chars(NewValue, NewChars),
	catch(term_to_atom(MeasureTerm, NewValue), _, fail),
	term_to_literal(MeasureTerm, Measure).

fix_dbpunits(Subject, Predicate, literal(type('http://sw.opencyc.org/concept/Mx4rvVjrc5wpEbGdrcN5Y29ycA', MinutesNew))) :-
	rdf(Subject, Predicate, literal(type(dbo:minute, Value))),
	atomic_list_concat([MinutesA, SecondsA], ':', Value),
	catch(atom_number(MinutesA, Minutes), _, fail),
	catch(atom_number(SecondsA, Seconds), _, fail),
	MinutesNew is Minutes + Seconds / 60.

term_to_literal(Term, literal(type(Type, Value))) :-
	Term =.. [Symbol, ValueN],
	atom_number(Value, ValueN),
	symbol_uri(Symbol, Type).

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

% Fix the junk in DBPedia. Some DBPedia number triples are stored as
% text, like "6,371.0 km"@en, which the system needs as a typed literal of "6371.0"^^dbo:kilometre
% This goal tries to parse the text representation and replace it with a typed interpretation.
fetch(Subject, Predicate, _, _) :-
	rdf_transaction((
		\+rdf(Subject, Predicate, literal(type(_, _))),
		rdfs_subproperty_of(DbProp, Predicate),
		rdf_global_id(dbp:_, DbProp),
		\+rdf(Subject, DbProp, _Object, user),
		solve(Subject, DbProp, [literal(lang(en, V)), DBPSubject | _]),
		fix_dbpunits(DBPSubject, DbProp, Literal),
		rdf_update(DBPSubject, DbProp, literal(lang(en, V)), object(Literal))
	)),
	fail.  % Force backtracking. This looks really weird, but it's necessary
	       % for the rest of the fetch plans to execute.

fetch(Subject, Predicate, _, _) :-
	rdf_transaction((
		rdf(Subject, Predicate, literal(type(dbo:minute, V))),
		\+rdf(Subject, Predicate, _Object, user),
		fix_dbpunits(Subject, Predicate, Literal),
		rdf_update(Subject, Predicate, literal(type(dbo:minute, V)), object(Literal))
	)), fail.

%%%%%%%%%%%%%% General purpose plans %%%%%%%%%%%%%%%
% Fetch epsilon value for a Class
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    rdf(Subject, Predicate, ObjectNode, user), !.

fetch(Class, Predicate, ObjectNode, user) :-
    \+rdf(Class, Predicate, ObjectNode, user),
    epsilon_exists(Class),
    rdf(TargetSubject, rdf:type, gu:'Epsilon'),
    rdf(TargetSubject, rdf:type, Class),
    rdf(TargetSubject, Predicate, ObjectNode), !.

fetch(Class, Predicate, ObjectNode, user) :-
    \+rdf(Class, Predicate, ObjectNode, user),
    epsilon_exists(Class, EpsilonNode),
    \+rdf(EpsilonNode, Predicate, ObjectNode),
    fetch(EpsilonNode, Predicate, ObjectNode, user).

fetch(Class, Predicate, ObjectNode, user) :-
	\+rdf(Class, Predicate, ObjectNode, user),
	epsilon_exists(Class),
	rdfs_individual_of(TargetSubject, gu:'Epsilon'),
	owl_individual_of(TargetSubject, Class),
	once((rdf(TargetSubject, rdf:type, Class),
    	Class \= 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/Epsilon')),
	rdf_has(TargetSubject, Predicate, ObjectNode).
	
% Fetch direct value for non-class, that has not already been stored in
% the user ontology
fetch(Subject, Predicate, ObjectNode, Graph) :-
    \+rdf(Subject, Predicate, _Object, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    rdf(Subject, Predicate, literal(type(Type, ObjectN)), Graph:_GraphID), !,
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

%% Geometrical Objects
fetch(Subject, Predicate, ObjectNode, user) :-
	\+rdf(Subject, Predicate, _Object, user),
	\+rdfs_individual_of(Subject, rdfs:'Class'),
	%rdfs_individual_of(Subject, ocyc:'Mx4rvVjpUZwpEbGdrcN5Y29ycA'), % Spatial thing
	( rdfs_individual_of(Predicate, ocyc:'Mx4rvVjuYJwpEbGdrcN5Y29ycA')
	 ; \+rdfs_individual_of(Predicate, ocyc:'Mx4rvVjuYJwpEbGdrcN5Y29ycA'),
	     rdfs_individual_of(Predicate, ocyc:'Mx4rvVi5xJwpEbGdrcN5Y29ycA')
	), % Measure 
	s_fetch(Subject, Predicate, ObjectNode, user).

%% Special plan for population
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _Object, user),
    is_region(Subject),
   	rdf_transaction(( find_value_for_region(Subject, Predicate, Value, _),
    store_statement(Subject, Predicate, literal(type(_, Value)), gu:'Function', ObjectNode))).

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

% Create new Epsilon node and feed back
fetch(Class, Predicate, ObjectNode, user) :-
    \+epsilon_exists(Class),
    \+rdfs_individual_of(Class, wns:'Synset'),
    rdfs_individual_of(Class, rdfs:'Class'),
    \+get_average_value(Class, Predicate, _),
    rdf_transaction((
    	rdf_node(EpsilonNode),
    	rdf_assert(EpsilonNode, rdf:type, Class),
    	rdf_assert(EpsilonNode, rdf:type, gu:'Epsilon'))),
    fetch(EpsilonNode, Predicate, ObjectNode, _).
    
fetch(Subject, Predicate, ObjectNode, user) :-
	rdfs_individual_of(Subject, gu:'Epsilon'),
	\+rdfs_individual_of(Subject, rdfs:'Class'),
	\+rdf(Subject, Predicate, _, user),
	once((rdf(Subject, rdf:type, Class),
    	Class \= 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/Epsilon')),
    get_average_value(Class, Predicate, type(Type, N)),
    store_statement(Subject, Predicate, literal(type(Type, N)), gu:'System', ObjectNode).
	
%%%%%%%%%%%%%% User Interaction Plans %%%%%%%%%%%%%%
% Ask the user for a value for an object
fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    \+rdf(Subject, rdf:type, gu:'Epsilon'),
    ask_user(Subject, Predicate, type(Type, N)),
    store_statement(Subject, Predicate, literal(type(Type, N)), gu:'CurrentUser', ObjectNode).

fetch(Subject, Predicate, ObjectNode, user) :-
    \+rdf(Subject, Predicate, _, user),
    \+rdfs_individual_of(Subject, rdfs:'Class'),
    rdf(Subject, rdf:type, gu:'Epsilon'),
    once((rdf(Subject, rdf:type, Class),
    	Class \= 'http://www.inf.ed.ac.uk/2009/06/01/guesstimation/Epsilon')),
    ask_user(Class, Predicate, type(Type, N)),
    store_statement(Subject, Predicate, literal(type(Type, N)), gu:'CurrentUser', ObjectNode).

%%%%%% Experimental, backtrack with superclass %%%%%%%
%fetch(EpsilonNode, Predicate, ObjectNode, user) :-
%	rdfs_individual_of(EpsilonNode, gu:'Epsilon'),
%	\+rdf(EpsilonNode, rdf:value, _),
%	rdf_transaction((
%		rdfs_individual_of(EpsilonNode, SuperClass1),
%		rdfs_subclass_of(SuperClass1, ocyc:'Mx4rvVj27ZwpEbGdrcN5Y29ycA'),
%		fetch(SuperClass1, Predicate, ObjectNode, user)
%	)).

%%%%%%%%%%%%%% End of Fetch plans %%%%%%%%%%%%%%%

/** epsilon_exists(?Class) is nondet.
Goal succeeds if Class refers to an RDFS class that already
has an epsilon node in the custom ontology.
*/
epsilon_exists(Class) :-
    epsilon_exists(Class, _).

epsilon_exists(Class, EpsilonNode) :-
	rdfs_individual_of(Class, rdfs:'Class'),
	rdfs_individual_of(EpsilonNode, gu:'Epsilon'),
    rdfs_individual_of(EpsilonNode, Class), !.

ask_user(Class, Property, type(Type, Value)) :-
    (cyclify(Class, EnglishClass) -> true ; EnglishClass = Class ),
    (cyclify(Property, EnglishProperty) -> true ; EnglishProperty = Property),
    prompt(_, 'ask> '),
    write('Need information about '), write(EnglishClass), write('.'), nl,
    (rdf(Class, rdfs:comment, literal(lang(en,Comment))) ->
    	writeln(Comment), nl ; true),
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
    geomean([Value, Value2], ValueGM),
    atom_number(ValueOut, ValueGM).