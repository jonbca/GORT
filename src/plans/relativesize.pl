%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains plans for relative size determination.

/* product_list(+List, -Result) is det.
   
   Returns the product of the numbers in List.
   
   @param List the numbers to multiply.
   @param Result the product of the numbers in List.
*/
product_list(List, Result) :-
	product_list(List, 1, Result).
    
product_list([], X, X).

product_list([H | Tail], PartialProduct, Product) :-
    atom(H),
    atom_number(H, N), !, 
    PartialProduct1 is PartialProduct * N,
    product_list(Tail, PartialProduct1, Product).

product_list([N | Tail], PartialProduct, Product) :-
    number(N),
    PartialProduct1 is PartialProduct * N,
    product_list(Tail, PartialProduct1, Product).
        
/* geomean(+List, -Result) is det.

   Returns the geometric mean of the numbers in List.
   
   @param List the list of numbers
   @param Result the geometric mean of the numbers.
*/
geomean(List, Result) :-
    product_list(List, Product),
    length(List, Count),
    Result is Product ** (1 / Count).
    