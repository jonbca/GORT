%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file defines the operators used throughout this project.

:- op(500, fx, ==>).
:- op(500, xfy, has_value).
:- op(500, xfy, for_predicate).
:- op(500, xfx, isa).
:- op(300, fx, 'derived by').
:- op(900, xfx, ::).

:- multifile (::)/2.