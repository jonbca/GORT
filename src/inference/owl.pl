%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for searching an RDF graph for triples
%% that satisfy a predicate, based on a starting node.
%%
%% Based on combination of code for DFS with closed set and returning path to solution
%% in:
%%
%% O'Keefe, R. A. 1990. The Craft of Prolog. Cambridge, Mass: MIT Press.
%% pp. 54 -- 68.

:- use_module(library('semweb/rdf_db')).

:- multifile noprove/1.
:- rdf_set_predicate(owl:'sameAs', transitive(true)),
   rdf_set_predicate(owl:'sameAs', symmetric(true)).

:- rdf_meta solve(r, r, -), solve(r, r, t, -), owl_individual_of(r,r).

noprove(solve).    % too verbose

owl_individual_of(Object1, Class) :-
	rdf(Object1, rdf:type, Class1),
	depth_first_star_labelled([Class1/[]], [Class1], Class/_).

/* solve(+Start, +Predicate, ?Solution) is multi.
Solve takes a Start node and target Predicate, and returns a solution path
to the value of that Predicate, following owl:sameAs relations. The Solution
path is a list beginning with the terminal value, and ending with the Start
node.

@param Start the start node
@param Predicate the predicate to search for
@param Solution the solution
*/
solve(Start, Predicate, [Literal, Answer | Path]) :-
    depth_first_star_labelled([Start/[]], [Start], Answer/Path),
    solution(Answer, Predicate, Literal, _).

solve(Start, Predicate, [Literal, Answer | Path], Graph) :-
    depth_first_star_labelled([Start/[]], [Start], Answer/Path),
    solution(Answer, Predicate, Literal, Graph).

/* depth_first_star_labelled(Open, Closed, Sol) is multi.
Performs a depth-first search on a graph starting with nodes
in the Open set, and never revisiting nodes in the Closed set.
The solution is of the form "TerminalNode / Path" where Path
is a list of the path followed by the algorithm to arrive to the TerminalNode.

@param Open the set of nodes yet to be searched
@param Closed the set of nodes that have been visited
@param Sol the solution path
*/
depth_first_star_labelled([X| _], _, X).
depth_first_star_labelled([X/RevPath | Open1], Closed, Y) :-
    children(X, Children),
    ord_union(Closed, Children, Closed1, Children1),
    add_paths(Children1, [X | RevPath], ChildrenWithPaths),
    append(ChildrenWithPaths, Open1, Open2),
    depth_first_star_labelled(Open2, Closed1, Y).

/* add_paths(+Nodes, +NodePath, -NewNodes) is det.
For all of the nodes in Nodes, appends /NodePath, and places the
result in NewNodes.

@param Nodes the nodes that need the new suffix
@param NodePath the suffix to append
@param NewNodes a new list with terms of the form Node/NodePath
*/
add_paths([], _, []).
add_paths([Node | Nodes], NodePath, [Node/NodePath | Rest]) :-
    add_paths(Nodes, NodePath, Rest).


/* children(+Node, -ChildrenSet) is det.
Finds all of the children of the start node in Node. ChildrenSet
is a duplicate-free list of the child nodes.

@param Node the node to start the search from
@param ChildrenSet a set of the child nodes of Node
*/
children(Node, ChildrenSet) :-
    findall(ChildNode,
    		child(Node, ChildNode),
    		ChildrenNodes),
    sort(ChildrenNodes, ChildrenSet).

/* child(?ParentNode, ?ChildNode) is multi.
True if ChildNode is a direct child of ParentNode, following an owl:sameAs
relation.

@param ParentNode the start node
@param ChildNode a node which is a direct descendant of ParentNode, by the owl:sameAs relation
*/
child(ParentNode, ChildNode) :-
    rdf_has(ParentNode, owl:'sameAs', ChildNode)
    ;
    rdf_has(ChildNode, owl:'sameAs', ParentNode).

/* solution(+Answer, +Predicate, ?Literal) is multi.
True if a solution exists that links Answer directly to Literal via Predicate
or one of its rdfs:subPropertyOf relations.

@param Answer the node to test
@param Predicate the predicate to test with
@literal the object of the predicate, whose presence indicates success
*/
solution(Answer, Predicate, Literal, Graph) :-
    rdf_has(Answer, Predicate, Literal, ActualPredicate),
    rdf(Answer, ActualPredicate, Literal, Graph).