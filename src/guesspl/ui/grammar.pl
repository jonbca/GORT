%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file defines the grammar for the user interface.


quantity --> [mass].%, volume, time, count, area, distance, length].
question_fragment --> [what_is_total].
of --> [of].
in --> [in].
place --> [A], {cyclify(_, A)}.
concept --> [blah].

sentence --> question_fragment, quantity, of, concept, in, place.