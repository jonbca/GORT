%%%% 
%% $Id$
%% 
%% MSc dissertation Guesstimation Project
%% By Jonathan Abourbih
%% (c) 2009 The University of Edinburgh
%%
%% This file contains logging code.

:- dynamic log_level/1.
:- multifile silent/1.

log_level(info).

% Establish relationships between logging levels. For example, INFO level
% means also print warn, error, and fatal messages
log_level(debug) :- log_level(info).
log_level(info) :- log_level(warn).
log_level(warn) :- log_level(error).
log_level(error) :- log_level(fatal).

% Don't even mention these in proofs
silent(log).
silent(debug).
silent(info).
silent(warn).
silent(error).
silent(fatal).
silent(set_log_level).

%% set_log_level(+Level) is det.
%
%  Sets the current level to Level. Level may be one of *debug, info, warn, error, fatal*.
%
%  @param Level the minimum level for logging.
set_log_level(Level) :-
    member(Level, [debug, info, warn, error, fatal]), !,
    retract(log_level(_)),
    assert(log_level(Level)).

set_log_level(Level) :-
    \+member(Level, [debug, info, warn, error, fatal]),
    error('Level must be one of [debug, info, warn, error, fatal]'), !.

%% log(+Level, +Message) is det.
%
%  Log a Message to the current output stream at the given Level.
%
%  @param Level the level of the log message
%  @param Message the message to record
log(Level, Message) :-
    member(Level, [debug, info, warn, error, fatal]), !, 
    Goal =.. [Level, Message],
    Goal.

log(Level, _) :-
    \+member(Level, [debug, info, warn, error, fatal]),
    error('Level must be one of [debug, info, warn, error, fatal]'), !.

%% log(+Level, +Message, +Goal) is det.
%
%  Log a Message and Goal at the appropriate Level. Goal must be a term.
%
%  @param Level the level of the log message 
%  @param Message the message to record
%  @param Goal a term to record with the message
log(Level, Message, Goal) :-
	member(Level, [debug, info, warn, error, fatal]), !, 
    LogGoal =.. [Level, Message, Goal],
    LogGoal.
   
log(Level, _, _) :-
    \+member(Level, [debug, info, warn, error, fatal]),
    error('Level must be one of [debug, info, warn, error, fatal]'), !.

%% debug(+Message) is det.
%
%  Records Message at the debug level, if debug is one of the currently-enabled levels.
%
%  @param Message the message to record.
debug(Message) :-
    log_level(debug),
    write('DEBUG: '),
    writeln(Message).

%% debug(+Message, +Goal) is det.
%
%  Records Message and Goal at the debug level, if debug is one of the currently-enabled levels.
%  Goal must be instantiated to a Prolog term.
%
%  @param Message the message to record.
debug(Message, Goal) :-
    log_level(debug),
    write('DEBUG: '),
    write(Message), write('; '),
    write_term(Goal, [max_depth(5), quoted(true)]).

%% info(+Message) is det.
%
%  Records Message at the info level, if info is one of the currently-enabled levels.
%
%  @param Message the message to record.
info(Message) :-
    log_level(info),
    write('INFO: '),
    writeln(Message).

%% info(+Message, +Goal) is det.
%
%  Records Message and Goal at the info level, if info is one of the currently-enabled levels.
%  Goal must be instantiated to a Prolog term.
%
%  @param Message the message to record.
info(Message, Goal) :-
    log_level(info),
    write('INFO: '),
    write(Message), write('; '),
    write_term(Goal, [max_depth(5), quoted(true)]).

%% warn(+Message) is det.
%
%  Records Message at the warn level, if warn is one of the currently-enabled levels.
%
%  @param Message the message to record.
warn(Message) :-
    log_level(warn),
    write('WARN: '),
    writeln(Message).

%% warn(+Message, +Goal) is det.
%
%  Records Message and Goal at the warn level, if warn is one of the currently-enabled levels.
%  Goal must be instantiated to a Prolog term.
%
%  @param Message the message to record.    
warn(Message, Goal) :-
    log_level(warn),
    write('WARN: '),
    write(Message), write('; '),
    write_term(Goal, [max_depth(5), quoted(true)]).

%% error(+Message) is det.
%
%  Records Message at the error level, if error is one of the currently-enabled levels.
%
%  @param Message the message to record.
error(Message) :-
    log_level(error),
    write('ERROR: '),
    writeln(Message).

%% error(+Message, +Goal) is det.
%
%  Records Message and Goal at the error level, if error is one of the currently-enabled levels.
%  Goal must be instantiated to a Prolog term.
%
%  @param Message the message to record.    
error(Message, Goal) :-
    log_level(error),
    write('ERROR: '),
    write(Message), write('; '),
    write_term(Goal, [max_depth(5), quoted(true)]).

%% fatal(+Message) is det.
%
%  Records Message at the fatal level, if fatal is one of the currently-enabled levels.
%
%  @param Message the message to record.    
fatal(Message) :-
    log_level(fatal),
    write('FATAL: '),
    writeln(Message).

%% fatal(+Message, +Goal) is det.
%
%  Records Message and Goal at the fatal level, if fatal is one of the currently-enabled levels.
%  Goal must be instantiated to a Prolog term.
%
%  @param Message the message to record.    
fatal(Message, Goal) :-
    log_level(fatal),
    write('FATAL: '),
    write(Message), write('; '),
    write_term(Goal, [max_depth(5), quoted(true)]).