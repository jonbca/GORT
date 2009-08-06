%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains reasoning for order-of-magnitude estimates
%% and calculations. 

noprove(to_om).

%%%% Avoid log(0) errors, represent 0 as om(0,0)
to_om(0, om(0, 0)) :- !.

%%%% Convert Value to om(M, E). Order of Magnitude is equivalent to
%%%% scientific notation representation of Value rounded to 1 significant figure.
%%%% e.g. 25000 --> 2.5e5 --> 3e5 --> om(3, 5).
to_om(Value, OOM) :-
    atom(Value), !,
    catch(atom_number(Value, ValueN), _, fail),
    to_om(ValueN, OOM).

to_om(ValueN, om(Mantissa, Exponent)) :-
    number(ValueN), !,
    ValueN \= 0,
    Exponent is floor(log10(abs(ValueN))),
    Mantissa is round(ValueN / (10 ** Exponent)).

to_om(om(E, M), om(E, M)).  % do nothing if it's already an OM

%%%% Convert from om(M, E); inverse of above function
from_om(Value, om(Mantissa, Exponent)) :-
    Value is Mantissa * 10 ** Exponent.

%%%%%% Multiplication Rules
% For case where the product of the mantissas is < 10,
% just add them together.
mult(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
	om(MantissaR, ExponentR)) :-
    Mantissa1 * Mantissa2 < 10,
    MantissaR is Mantissa1 * Mantissa2,
    ExponentR is Exponent1 + Exponent2.
    
% For the case where the product of the mantissas is >= 10,
% divide by 10 and add 1 to the exponent.
mult(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
	om(MantissaR, ExponentR)) :-
    Mantissa1 * Mantissa2 >= 10,
    MantissaR is round((Mantissa1 * Mantissa2)/10),
    ExponentR is Exponent1 + Exponent2 + 1.

%%%%%% Division Rules
% For the case where the quotient of the mantissas is >= 1,
% divide the mantissas and round to the nearest integer.
div(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
    om(MantissaR, ExponentR)) :-
    Mantissa1 / Mantissa2 >= 1,
    MantissaR is round(Mantissa1 / Mantissa2),
    ExponentR is Exponent1 - Exponent2.

% For the case where the quotient of the mantissas is < 1,
% multiply by 10 and subtract 1 from the exponent.
div(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
    om(MantissaR, ExponentR)) :-
    Mantissa1 / Mantissa2 < 1,
    MantissaR is round((Mantissa1 / Mantissa2) * 10),
    ExponentR is Exponent1 - Exponent2 - 1.

%%%%%% Addition Rules

%%%% Addition where the 2nd argument is bigger. This returns
%%%% the second argument only; it dominates the addition.
sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
        om(Mantissa2, Exponent2)) :-
    Exponent1 < Exponent2,
    sign(Mantissa1) =:= sign(Mantissa2).

%%%% Addition where the first argument is bigger. This returns
%%%% the first argument only; it dominates the addition.
sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
        om(Mantissa1, Exponent1)) :-
    Exponent1 > Exponent2,
    sign(Mantissa1) =:= sign(Mantissa2).

%%%% Addition where the exponents are the same, and the sum
%%%% of the mantissas is less than 10.
sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
        om(MantissaR, Exponent1)) :-
    Exponent1 == Exponent2,
    sign(Mantissa1) =:= sign(Mantissa2),
    Mantissa1 + Mantissa2 < 10,
    MantissaR is Mantissa1 + Mantissa2.

%%%% Addition where the exponents are the same, but the sum
%%%% of the mantissas is greater than 10. Increments the exponent
%%%% and picks a new mantissa as appropriate.
sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
        om(MantissaR, ExponentR)) :-
    Exponent1 == Exponent2,
    sign(Mantissa1) =:= sign(Mantissa2),
    Mantissa1 + Mantissa2 >= 10,
    ExponentR is Exponent1 + 1,
    MantissaR is round((Mantissa1 + Mantissa2)/10).
    
%%%% Next two rules handle the case where the signs are
%%%% different between the two addends. Just maps to
%%%% the diff/3 functors below.
sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
 		Diff) :-
    sign(Mantissa1) < sign(Mantissa2),
    diff(om(Mantissa2, Exponent2), om(-Mantissa1, Exponent1),
    	Diff).

sum(om(Mantissa1, Exponent1), om(Mantissa2, Exponent2),
 		Diff) :-
    sign(Mantissa1) > sign(Mantissa2),
    diff(om(Mantissa1, Exponent1), om(-Mantissa2, Exponent2),
    	Diff).
    	
%%%%%% Subtraction Rules
% Represent 0 as om(0, 0)
diff(A, A, om(0, 0)).

%%%% Compute the difference between the two arguments. Does not
%%%% make explicit the rules as above for summation. Instead,
%%%% converts to numeric representation and does the subtraction.
%%%% This will need to be adapted to handle cases where mantissa is
%%%% unknown.
diff(Minuend, Subtrahend, Result) :-
    Minuend \= Subtrahend,
    from_om(MinuendValue, Minuend),
    from_om(SubtrahendValue, Subtrahend),
    Diff is MinuendValue - SubtrahendValue,
    to_om(Diff, Result).
    