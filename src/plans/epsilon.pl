%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains rules for calculating a 'typical' example of an entity
%% and property.

:- use_module(library('semweb/rdf_db')).

:- multifile noprove/1.
:- dynamic current_user/1.
:- rdf_meta epsilon(r, r, -, r), get_items(r, r, t).

get_average_value(Class, Property, type(DesiredType, Average)) :-
    findall(type(Type, Value), get_items(Class, Property, type(Type, Value)), Values),
    convert_list(Values, DesiredType, ValuesN),
    average_list(ValuesN, Average).

/**get_items(+Domain, +Property, type(?Type, -Value)) is multi.

Returns a value for the Property for all items in the domain that match Type.
If Type is not instantiated, it returns all of the items in the domain with their
types. If Type is instantiated, it will return all of the items in the domain with
the property converted to the type specified in the Type parameter.

@param Domain the domain for which to find individuals
@param Property the property whose value is desired
@param Type the type of the returned data in Value
@param Value the value of the property.
*/
get_items(Domain, Property, type(Type, Value)) :-
    var(Type),
    rdfs_individual_of(Item, Domain),
    (fix_dbpunits(Item, Property, ValueType) ; rdf_has(Item, Property, ValueType)),
    ValueType = literal(type(Type, Value)).

get_items(Domain, Property, type(Type, Value)) :-
    \+var(Type),
    rdfs_individual_of(Item, Domain),
    rdf_has(Item, Property, literal(ValueType)),
    ValueType = type(NativeType, NativeValue),
    (atom_number(NativeValue, NativeValueN) ; number(NativeValue), NativeValueN = NativeValue),
    convert(NativeValueN, NativeType, Value, Type).    %%FIXME
    

/**convert_list(+InputList, +TargetUnits, -OutputList) is det.
Converts type(Type, Value) pairs in the InputList into the TargetUnits, if
convertible. The converted value is stored as a number in the OutputList.

@param InputList the list to convert
@param TargetUnits the desired units
@param OutputList the converted values
*/
convert_list([], _, []).
convert_list([type(Type, Value) | Tail], TargetUnits, [NewValue | NewTail]) :-
    do_conversion(Value, Type, NewValue, TargetUnits),
    convert_list(Tail, TargetUnits, NewTail).
convert_list( [type(Type, Value) | Tail], TargetUnits, New ) :-
    \+do_conversion(Value, Type, _, TargetUnits),
    convert_list(Tail, TargetUnits, New).
    
do_conversion(Value, Type, NewValue, TargetUnits) :-
    number(Value),
    var(TargetUnits),
    rdf(TargetUnits, gu:scaleFactor, literal(type(xsd:float, '1.0'))),
    same_dimension(TargetUnits, Type), !,
    convert(Value, Type, NewValue, TargetUnits).     %%FIXME
    
do_conversion(Value, Type, NewValue, TargetUnits) :-
	\+number(Value),
	var(TargetUnits),
	rdf(CycUnits, owl:sameAs, Type),
	rdf(TargetUnits, gu:scaleFactor, literal(type(xsd:float, '1.0'))),
	same_dimension(CycUnits, TargetUnits),
    !,
    catch(
    	atom_number(Value, ValueN),
    	_,
    	fail
    ),
    convert(ValueN, Type, NewValue, TargetUnits).    %%FIXME

do_conversion(Value, Type, NewValue, TargetUnits) :-
	\+number(Value),
	\+var(TargetUnits),
    catch(
    	atom_number(Value, ValueN),
    	_,
    	fail
    ),
    convert(ValueN, Type, NewValue, TargetUnits). 
/** average_list(+List, -Average) is det.
Returns the average value of all elements in a list.

@param List the list to average
@param Average the average of the items in the list
*/
average_list(List, Average) :-
    sum_list(List, Sum),
    length(List, Length),
    Length \= 0,
    Average is Sum / Length.