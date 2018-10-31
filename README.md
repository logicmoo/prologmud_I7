# SmallAdventureGames
Small adventure Games Gleaned off the Web

CIS 554 Programming Paradigms   http://www.cis.upenn.edu/~matuszek/cis554-2014/index.html
Fall 2014, David Matuszek

# Marty's Prolog Adventure Prototype
Marty's Interactive Fiction Engine in Prolog under marty_white
````
?- ensure_loaded(library(nomic_mu)).

````


Featuring a chatbot named Floyd whom uses

# The classical CHAT80 natural language system

The CHAT80 system has been developed in the 70s and 80s by Fernando C.N.
Pereira and David H.D. Warren. It implements a natural language question
answering system that answers  questions   about  the  world: countries,
cities, rivers, etc. It does so by   parsing the question, translate the
parse to a Prolog query and run this against its database.

This version is derived from the original  via Quintus Prolog after some
compatibility modifications for SWI-Prolog and   adding  a module header
that allows using it safely together with other applications.

The code is definitely dated. Still, it   provides  a nice example using
Prolog for parsing, assigning meaning and querying.

## Legal

The copyright is as far as we know   with  the original authors and made
available under a classical _academic use license_. See `LICENSE` in the
`prolog/chat80` directory. The content of that  file was copied from the
Python [NLTK data package](https://www.kaggle.com/nltkdata/chat-80/home)
that includes the chat80 files.

## INSTALL

````
tmptest@gitlab:~$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- pack_install(small_adventure_games).
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Install small_adventure_games@1.1.8 from GIT at https://github.com/TeamSPoon/small_adventure_games.git Y/n?

Create directory for packages
   (1) * /home/tmptest/lib/swipl/pack
   (2)   Cancel

Your choice? 1
% Cloning into '/home/tmptest/lib/swipl/pack/small_adventure_games'...
% Checking out files:  82% (385/466)
% Checking out files:  83% (387/466)
...
% Checking out files: 100% (466/466)
% Checking out files: 100% (466/466), done.
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Warning: Package depends on the following:
Warning:   "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:   "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:     "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:   "no_repeats", provided by no_repeats@1.1.118 from https://github.com/TeamSPoon/no_repeats.git
Warning:   "pfc", provided by pfc@1.1.118 from https://github.com/TeamSPoon/pfc.git
Warning:     "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "file_scope", provided by file_scope@1.1.118 from https://github.com/TeamSPoon/file_scope.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "hook_hybrid", provided by hook_hybrid@1.1.118 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:           "file_scope", provided by file_scope@1.1.118 from https://github.com/TeamSPoon/file_scope.git
Warning:           "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:             "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:               "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:           "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:           "subclause_expansion", provided by subclause_expansion@1.1.118 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:             "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:           "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:             "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "xlisting", provided by xlisting@1.1.118 from https://github.com/TeamSPoon/xlisting.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:     "hook_hybrid", provided by hook_hybrid@1.1.118 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "file_scope", provided by file_scope@1.1.118 from https://github.com/TeamSPoon/file_scope.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "hook_hybrid", provided by hook_hybrid@1.1.118 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:           "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:             "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:               "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:           "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:             "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:           "xlisting", provided by xlisting@1.1.118 from https://github.com/TeamSPoon/xlisting.git
Warning:             "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:               "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:                 "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "hook_hybrid", provided by hook_hybrid@1.1.118 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:           "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:             "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "xlisting", provided by xlisting@1.1.118 from https://github.com/TeamSPoon/xlisting.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "subclause_expansion", provided by subclause_expansion@1.1.118 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:         "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:       "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "hook_hybrid", provided by hook_hybrid@1.1.118 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "file_scope", provided by file_scope@1.1.118 from https://github.com/TeamSPoon/file_scope.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:           "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:         "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:           "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:             "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "must_trace", provided by must_trace@1.1.118 from https://github.com/TeamSPoon/must_trace.git
Warning:         "subclause_expansion", provided by subclause_expansion@1.1.118 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.118 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "loop_check", provided by loop_check@1.1.118 from https://github.com/TeamSPoon/loop_check.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "xlisting", provided by xlisting@1.1.118 from https://github.com/TeamSPoon/xlisting.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:     "no_repeats", provided by no_repeats@1.1.118 from https://github.com/TeamSPoon/no_repeats.git
Warning:     "s_expression", provided by s_expression@1.1.118 from https://github.com/TeamSPoon/s_expression.git
Warning:       "with_open_options", provided by with_open_options@1.1.118 from https://github.com/TeamSPoon/with_open_options.git
Warning:     "with_thread_local", provided by with_thread_local@1.1.118 from https://github.com/TeamSPoon/with_thread_local.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.118 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "xlisting", provided by xlisting@1.1.118 from https://github.com/TeamSPoon/xlisting.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.118 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.118 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.118 from https://github.com/TeamSPoon/gvar_syntax.git

What do you wish to do
   (1) * Install proposed dependencies
   (2)   Only install requested package
   (3)   Cancel

Your choice? 1
% "small_adventure_games.git" was downloaded 1 times
% Cloning into '/home/tmptest/lib/swipl/pack/each_call_cleanup'...
i each_call_cleanup@1.1.118 - Each Call Redo Setup and Cleanup
% Updating index for library /home/tmptest/lib/swipl/pack/each_call_cleanup/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/loop_check'...
i loop_check@1.1.118        - New simple loop checking
% Updating index for library /home/tmptest/lib/swipl/pack/loop_check/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/with_thread_local'...
i with_thread_local@1.1.118 - Call a Goal with local assertions
% Updating index for library /home/tmptest/lib/swipl/pack/with_thread_local/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/no_repeats'...
i no_repeats@1.1.118        - New ways to avoid duplicate solutions
% Updating index for library /home/tmptest/lib/swipl/pack/no_repeats/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/pfc'...
i pfc@1.1.118               - Pfc -- a package for forward chaining in Prolog
% Cloning into '/home/tmptest/lib/swipl/pack/file_scope'...
i file_scope@1.1.118        - File local scoped efects
% Updating index for library /home/tmptest/lib/swipl/pack/file_scope/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/logicmoo_utils'...
i logicmoo_utils@1.1.118    - Common predicates used by external Logicmoo Utils and Base
% Updating index for library /home/tmptest/lib/swipl/pack/logicmoo_utils/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/must_trace'...
i must_trace@1.1.118        - Trace with your eyeballs instead of your fingers
% Updating index for library /home/tmptest/lib/swipl/pack/must_trace/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/clause_attvars'...
i clause_attvars@1.1.118    - An alternate interface to the clause database to allow attributed variables to be asserted
% Updating index for library /home/tmptest/lib/swipl/pack/clause_attvars/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/dictoo'...
i dictoo@1.1.118            - Dict-like OO Syntax
% Cloning into '/home/tmptest/lib/swipl/pack/gvar_syntax'...
i gvar_syntax@1.1.118       - Global Variable Syntax
% Cloning into '/home/tmptest/lib/swipl/pack/hook_hybrid'...
i hook_hybrid@1.1.118       - Hook assert retract call of *specific* predicates
% Updating index for library /home/tmptest/lib/swipl/pack/hook_hybrid/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/subclause_expansion'...
i subclause_expansion@1.1.118 - More use specific versions of term/goal expansion hooks
% Updating index for library /home/tmptest/lib/swipl/pack/subclause_expansion/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/xlisting'...
i xlisting@1.1.118          - Selective Interactive Non-Deterministic Tracing
% Updating index for library /home/tmptest/lib/swipl/pack/xlisting/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/s_expression'...
i s_expression@1.1.118      - Utilities for Handling of S-Expression Lisp/Scheme-Like forms and parsing of KIF, GDL, PDDL, CLIF
% Updating index for library /home/tmptest/lib/swipl/pack/s_expression/prolog/
% Cloning into '/home/tmptest/lib/swipl/pack/with_open_options'...
i with_open_options@1.1.118 - Utilities to open various objects for read/write
% Updating index for library /home/tmptest/lib/swipl/pack/with_open_options/prolog/
Package:                small_adventure_games
Title:                  PrologMUD I7 (NomicMU!) with small Adventure Games in Prolog using the CHAT80 Prolog natural language application
Installed version:      1.1.8
Author:                 Douglas R. Miles <logicmoo@gmail.com>, Marty White <lmw22@cornell.edu>, Fernando C.N. Pereira <unknown@unknown>, David H.D. Warren <unknown@unknown>, Jan Wielemaker <J.Wielemaker@vu.nl>
Maintainer:             Douglas R, Miles <logicmoo@gmail.com>
Home page:              https://github.com/TeamSPoon/prologmud_I7.git
Download URL:           https://github.com/TeamSPoon/prologmud_I7/archive/V*.zip
Requires:               each_call_cleanup, no_repeats, loop_check, pfc
Activate pack "small_adventure_games" Y/n?
true.

?-
````

## Use the module

````
?- use_module(library(nomic_mu)).
Loading tt0_00022_cycl.qlf  ...
% 28,465,413 inferences, 7.916 CPU in 7.917 seconds (100% CPU, 3596134 Lips)
Loading ac_xnl_7166.qlf (this may take 60-120 seconds the very first time) ...
% 201,097,784 inferences, 52.392 CPU in 53.161 seconds (99% CPU, 3838316 Lips)
Loading clex_nldata.qlf ...
% 405,917 inferences, 0.340 CPU in 0.340 seconds (100% CPU, 1194484 Lips)
spy(+Pred)
    Put  a spy point on all predicates meeting the  predicate specifica-
    tion Pred.  See section ??.

nospy(+Pred)
    Remove   spy  point  from  all  predicates  meeting   the  predicate
    specification Pred.


% ensure_loaded(adv_agents).

% ensure_loaded(adv_robot_floyd).


You may start the server with:

 ?- srv_mu.

true.

?-
````

## Run it

````
?- srv_mu.

?- srv_mu.

% adv_server(2666).

Server is starting on port 2666
%  Thread Status       Time    Stack use    allocated
% ---------------------------------------------------
%    main running    86.356        4,224      137,192
%      gc running     1.591          816      120,808
% mu_2666 running     0.000          760      120,808

% planner:'ORDERING TEST:~n'.

%   unordered was {6,[before(start,finish),before(start,x),before(start,y),before(y,finish),before(x,z)],...(_6818)}
player~1@spatial>
%   ordering is {8,[before(z,finish),before(x,finish),before(x,z),before(start,z),before(y,finish)],...(_7302)}
player~1@spatial>
%   picked [start,x,y,z,finish]
player~1@spatial>
%   picked [start,x,z,y,finish]
player~1@spatial>
%   picked [start,y,x,z,finish]
player~1@spatial>
% planner:'  END ORDERING TEST~n'.

% iObjectList=['cabinate~1','cup~1','sink~1','plate~1','table~1','lamp~1','bowl~1','flour~1','box~1','table_leg~1','videocamera~1','shovel~1','mushroom~1','fountain~1','rock~1','shelf~1','floyd~1','player~1','coins~1',basement,dining_room,garden,kitchen,living_room,pantry,'bag~1',brklamp,screendoor].

% create_object('cabinate~1',[inherit(cabinate,t),inherit(cabinate,t)]).

% create_object('cup~1',[inherit(cup,t)]).

% create_object('sink~1',[inherit(sink,t),inherit(sink,t)]).

% create_object('plate~1',[inherit(plate,t)]).

% create_object('table~1',[inherit(table,t),inherit(table,t),inherit(table,t),inherit(table,t)]).

% create_object('lamp~1',[inherit(lamp,t)]).

% create_object('bowl~1',[inherit(bowl,t),inherit(bowl,t)]).

% create_object('flour~1',[inherit(flour,t)]).

% create_object('box~1',[inherit(box,t),inherit(box,t)]).

% create_object('table_leg~1',[inherit(table_leg,t)]).

% create_object('videocamera~1',[inherit(videocamera,t)]).

% create_object('shovel~1',[inherit(shovel,t)]).

% create_object('mushroom~1',[inherit(mushroom,t)]).

% create_object('fountain~1',[inherit(fountain,t)]).

% create_object('rock~1',[inherit(rock,t)]).

% create_object('shelf~1',[inherit(shelf,t)]).

% create_object('floyd~1',[name('Floyd the robot'),inherit(autonomous,t),inherit(robot,t)]).

% create_object('player~1',[name($self),inherit(console,t),inherit(humanoid,t)]).

% create_object('coins~1',[inherit(coins,t)]).

% create_object(basement,[inherit(place,t),desc('This is a very dark basement.'),state(dark,t)]).

% create_object(dining_room,[inherit(place,t)]).

% create_object(garden,[inherit(place,t),goto(_936,_938,up,'You lack the ability to fly.'),effect(goto(_936,_938,_962,north),getprop(screendoor,state(opened,t))),oper($self,goto(_936,_938,_1004,north),precond(getprop(screendoor,state(opened,t)),['you must open the door first']),body(inherited)),cant_goto(_936,_938,'The fence surrounding the garden is too tall and solid to pass.')]).

% create_object(kitchen,[inherit(place,t)]).

% create_object(living_room,[inherit(place,t)]).

% create_object(pantry,[inherit(place,t),nouns(closet),nominals(kitchen),desc('You\'re in a dark pantry.'),state(dark,t)]).

% create_object('bag~1',[inherit(bag,t)]).

% create_object(brklamp,[inherit(broken,t),name('possibly broken lamp'),effect(switch(on),print_("Switch is flipped")),effect(hit,[print_("Hit brklamp"),setprop($self,inherit(broken,t))]),inherit(lamp,t)]).

% create_object(screendoor,[door_to(kitchen),door_to(garden),state(opened,f),inherit(door,t)]).
=============================================
INIT STATE
=============================================
[ props(screendoor,
        [ door_to(kitchen),
          door_to(garden),
          state(opened,f),
          can_be(move,f),
          can_be(open,t),
          can_be(close,t),
          nouns(door),
          can_be(touch,t),
          can_be(examine(spatial),t),
          class_desc(['kind is corporial','kind is normally thinkable']),
          inherited(thinkable),
          inherited(corporial),
          inherited(door),
          co([ door_to(kitchen),
               door_to(garden),
               state(opened,f),
               inherit(door,t)
             ])
        ]),
 ...<CLIPPED>...  

 props('sink~1',
        [ oper('sink~1',
               put(spatial,Q,in,'sink~1'),
               precond(~(getprop(Q,inherit(liquid,t))),
                       ['liquids would spill out']),
               body(move(spatial,Q,in,'sink~1'))),
          has_rel(spatial,in),
          inherited(container),
          can_be(touch,t),
          can_be(move,f),
          can_be(examine(spatial),t),
          class_desc(['kind is furnature','kind is normally thinkable']),
          inherited(thinkable),
          inherited(furnature),
          volume_capacity(10),
          state(dirty,t),
          inherit(flask,f),
          inherited(sink),
          co([inherit(sink,t),inherit(sink,t)])
        ]),
  props('cup~1',
        [ oper('cup~1',
               put(spatial,R,in,'cup~1'),
               precond(getprop(R,inherit(corporial,t)),
                       ['non-physical would spill out']),
               body(move(spatial,R,in,'cup~1'))),
          can_be(touch,t),
          can_be(examine(spatial),t),
          class_desc(['kind is an Object','kind is normally thinkable']),
          inherited(thinkable),
          can_be(move,t),
          inherited(object),
          oper('cup~1',
               put(spatial,S,in,'cup~1'),
               precond(~(getprop(S,inherit(liquid,t))),
                       ['liquids would spill out']),
               body(move(spatial,S,in,'cup~1'))),
          has_rel(spatial,in),
          inherited(container),
          inherited(flask),
          inherited(cup),
          co([inherit(cup,t)])
        ]),
  props('cabinate~1',
        [ oper('cabinate~1',
               put(spatial,T,in,'cabinate~1'),
               precond(~(getprop(T,inherit(liquid,t))),
                       ['liquids would spill out']),
               body(move(spatial,T,in,'cabinate~1'))),
          has_rel(spatial,in),
          inherited(container),
          can_be(touch,t),
          can_be(move,f),
          can_be(examine(spatial),t),
          class_desc(['kind is furnature','kind is normally thinkable']),
          inherited(thinkable),
          inherited(furnature),
          volume_capacity(10),
          inherited(cabinate),
          co([inherit(cabinate,t),inherit(cabinate,t)])
        ]),
  h(spatial,in,'cup~1','cabinate~1'),
  h(spatial,in,'cabinate~1',kitchen),
  h(spatial,in,'plate~1','sink~1'),
  h(spatial,in,'sink~1',kitchen),
  h(spatial,on,'lamp~1','table~1'),
  h(spatial,in,'table~1',kitchen),
  h(spatial,in,'flour~1','bowl~1'),
  h(spatial,in,'bowl~1','box~1'),
  h(spatial,on,'box~1','table~1'),
  h(spatial,reverse(on),'table~1','table_leg~1'),
  h(spatial,in,'videocamera~1',living_room),
  h(spatial,in,'shovel~1',basement),
  h(spatial,in,'mushroom~1',garden),
  h(spatial,in,'fountain~1',garden),
  h(spatial,in,'rock~1',garden),
  h(spatial,in,'shelf~1',pantry),
  structure_label(istate),
  memories('player~1',
           [ structure_label(mem('player~1')),
             timestamp(0,1650.3),
             model([]),
             goals([]),
             todo([look]),
             inst('player~1'),
             name('player~1'),
             inherit(console,t),
             inherit(humanoid,t)
           ]),
  h(spatial,in,'floyd~1',pantry),
  h(spatial,in,'player~1',kitchen),
  h(spatial,worn_by,'watch~1','player~1'),
  h(spatial,held_by,'bag~1','player~1'),
  h(spatial,in,'coins~1','bag~1'),
  h(spatial,held_by,'wrench~1','floyd~1'),
  h(spatial,exit(south),pantry,kitchen),
  h(spatial,exit(north),kitchen,pantry),
  h(spatial,exit(down),pantry,basement),
  h(spatial,exit(up),basement,pantry),
  h(spatial,exit(south),kitchen,garden),
  h(spatial,exit(north),garden,kitchen),
  h(spatial,exit(east),kitchen,dining_room),
  h(spatial,exit(west),dining_room,kitchen),
  h(spatial,exit(north),dining_room,living_room),
  h(spatial,exit(east),living_room,dining_room),
  h(spatial,exit(south),living_room,kitchen),
  h(spatial,exit(west),kitchen,living_room),
  h(spatial,in,screendoor,kitchen),
  h(spatial,in,screendoor,garden),
  h(spatial,in,brklamp,garden)
]

=============================================
Welcome to Marty's Prolog Adventure Prototype
=============================================

% floyd~1 @ somewhere: about to: look
player~1@spatial>
% floyd~1 @ somewhere: about to: examine(see,shelf~1)
player~1@spatial>
Player~1 see: Player~1 are in the kitchen
Exits are north, south, east and west.
Player~1 see: cabinate, sink, table and screendoor.
player~1@spatial>
% floyd~1: sense_props(see,'shelf~1',[has_rel(spatial,on),can_be(move,f),inherited(shelf),co([inherit(shelf,t)])])
player~1@spatial>
% floyd~1: sense_props(see,'floyd~1',[name('Floyd the robot'),can_do(eat,f),inherited(autonomous),emitting(see,light),mass(200),nouns(robot),adjs(metallic),desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),can_be(switch,t),adjs(shiny),can_be(move,t),class_desc(['kind is both partly corporial and non-corporial','kind is completely non-corporial','kind is normally thinkable','kind is corporial','kind is an Object']),inherited(object),inherited(shiny),has_rel(spatial,held_by),volume(50),can_do(examine,t),can_do(touch,t),can_be(touch,t),can_be(examine(spatial),t),inherited(corporial),inherited(thinkable),inherit(corporial,f),inherited(noncorporial),inherited(partly_noncorporial),inherited(character),state(powered,t),effect(switch(on),setprop('floyd~1',state(powered,t))),effect(switch(off),setprop('floyd~1',state(powered,f))),inherited(robot),co([name('Floyd the robot'),inherit(autonomous,t),inherit(robot,t)])])
player~1@spatial>
% failed_update_model('floyd~1',failure(take('floyd~1'),cant(manipulate(_552,self))),4).

% failed_update_model('floyd~1',true,5).

% floyd~1: sense_props(see,'shovel~1',[inherited(shovel),co([inherit(shovel,t)])])
player~1@spatial>
the floyd arrives from the north
player~1@spatial>
% floyd~1: sense_props(see,'cabinate~1',[oper('cabinate~1',put(spatial,_662,in,'cabinate~1'),precond(~(getprop(_662,inherit(liquid,t))),['liquids would spill out']),body(move(spatial,_662,in,'cabinate~1'))),has_rel(spatial,in),inherited(container),can_be(touch,t),can_be(move,f),can_be(examine(spatial),t),class_desc(['kind is furnature','kind is normally thinkable']),inherited(thinkable),inherited(furnature),volume_capacity(10),inherited(cabinate),co([inherit(cabinate,t),inherit(cabinate,t)])])
player~1@spatial>
% floyd~1: sense_props(see,'sink~1',[oper('sink~1',put(spatial,_124,in,'sink~1'),precond(~(getprop(_124,inherit(liquid,t))),['liquids would spill out']),body(move(spatial,_124,in,'sink~1'))),has_rel(spatial,in),inherited(container),can_be(touch,t),can_be(move,f),can_be(examine(spatial),t),class_desc(['kind is furnature','kind is normally thinkable']),inherited(thinkable),inherited(furnature),volume_capacity(10),state(dirty,t),inherit(flask,f),inherited(sink),co([inherit(sink,t),inherit(sink,t)])])
player~1@spatial>
% floyd~1: sense_props(see,'table~1',[has_rel(spatial,on),inherited(table),co([inherit(table,t),inherit(table,t),inherit(table,t),inherit(table,t)])])
player~1@spatial>
% floyd~1: sense_props(see,'player~1',[name('player~1'),inherited(console),has_rel(spatial,held_by),mass(50),volume(50),can_do(eat,t),can_do(examine,t),can_do(touch,t),can_be(touch,t),can_be(examine(spatial),t),class_desc(['kind is both partly corporial and non-corporial','kind is completely non-corporial','kind is normally thinkable','kind is corporial']),inherited(corporial),inherited(thinkable),inherit(corporial,f),inherited(noncorporial),inherited(partly_noncorporial),inherited(character),inherited(player),can_be(switch(off),f),state(powered,t),inherited(humanoid),co([name($self),inherit(console,t),inherit(humanoid,t)])])
player~1@spatial>
% floyd~1: sense_props(see,screendoor,[door_to(kitchen),door_to(garden),state(opened,f),can_be(move,f),can_be(open,t),can_be(close,t),nouns(door),can_be(touch,t),can_be(examine(spatial),t),class_desc(['kind is corporial','kind is normally thinkable']),inherited(thinkable),inherited(corporial),inherited(door),co([door_to(kitchen),door_to(garden),state(opened,f),inherit(door,t)])])
player~1@spatial>
[,floyd] goes west
player~1@spatial>
% floyd~1: sense_props(see,'videocamera~1',[can_be(switch,t),effect(switch(on),setprop('videocamera~1',state(powered,t))),effect(switch(off),setprop('videocamera~1',state(powered,f))),state(powered,t),fragile(broken_videocam),inherited(videocamera),co([inherit(videocamera,t)])])
player~1@spatial>
the floyd arrives from the west
player~1@spatial>
[,floyd] goes east
player~1@spatial>
````

## HELP COMMAND

````
player~1@spatial> help
help
:- dynamic cmd_help/2.

cmd_help(quit, "Quits the game.").
cmd_help(english, "english <level>: turn on paraphrase generation.").
cmd_help(rtrace, "Debbuging: Start the non-interactive tracer.").
cmd_help(nortrace, "Debbuging: Stop the non-interactive tracer.").
...
cmd_help(possess(agent), "Take possession of a character").

player~1@spatial>
````

## AUTO COMMAND

player~1@spatial> a
a
auto
player~1@spatial>
the player~1 arrives from the west
player~1@spatial>
Player~1 see: Player~1 are in the kitchen
Exits are north, south, east and west.
Player~1 see: floyd, cabinate, sink, table and screendoor.
player~1@spatial> a
a
auto
player~1@spatial>
the player~1 arrives from the west
player~1@spatial>
Player~1 see: Player~1 are in the dining_room
Exits are west and north.
Player~1 see: <nothing>
player~1@spatial>
the floyd arrives from the west
player~1@spatial>
````

## MEMORY COMMAND


````
player~1@spatial> memory
memory
memory(player~1)
[ todo([]),
  [ emoted(see,
           'floyd~1',
           *,
           [cap(subj('floyd~1')),'inspects their inspection cover.'])
  ],
  model([ h(spatial,in,'floyd~1',dining_room,13),
          h(spatial,exit(north),dining_room,'<unexplored>',13),
          h(spatial,exit(west),dining_room,'<unexplored>',13),
          h(spatial,in,'player~1',dining_room,13),
          h(spatial,exit(east),kitchen,dining_room,13),
          h(spatial,exit(west),kitchen,living_room,10),
          h(spatial,exit(south),kitchen,'<unexplored>',10),
          h(spatial,exit(north),kitchen,'<unexplored>',10),
          h(spatial,in,screendoor,kitchen,10),
          h(spatial,in,'table~1',kitchen,10),
          h(spatial,in,'sink~1',kitchen,10),
          h(spatial,in,'cabinate~1',kitchen,10),
          h(spatial,exit(south),living_room,kitchen,10),
          h(spatial,exit(east),living_room,'<unexplored>',8),
          h(spatial,in,'videocamera~1',living_room,8)
        ]),
  timestamp(14,1926.7),
  [ moved('player~1',kitchen,in,dining_room),
    the('player~1'),
    person(arrived,arrives),
    from,
    the,
    west
  ],
  [ sense(see,
          [ you_are(in,dining_room),
            exits_are([west,north]),
            here_are(['player~1'])
          ])
  ],
  [ moved('floyd~1',kitchen,in,dining_room),
    the('floyd~1'),
    person(arrived,arrives),
    from,
    the,
    west
  ],
  timestamp(13,1845.6),
  did(goto(walk,east,L38,M38)),
  goals([]),
  [ emoted(see,
           'floyd~1',
           *,
           [cap(subj('floyd~1')),'hums quietly to themself.'])
  ],
  timestamp(12,1835.3),
  [ emoted(see,
           'floyd~1',
           *,
           [cap(subj('floyd~1')),'inspects their inspection cover.'])
  ],
  timestamp(11,1825.1),
  [ moved('player~1',living_room,in,kitchen),
    the('player~1'),
    person(arrived,arrives),
    from,
    the,
    west
  ],
  [ sense(see,
          [ you_are(in,kitchen),
            exits_are([north,south,east,west]),
            here_are([ 'player~1',
                       'floyd~1',
                       'cabinate~1',
                       'sink~1',
                       'table~1',
                       screendoor
                     ])
          ])
  ],
  timestamp(10,1814.6),
  did(goto(walk,south,L38,M38)),
  [time_passes],
  timestamp(9,1812.3),
  did(wait),
  [ moved('player~1',kitchen,in,living_room),
    the('player~1'),
    person(arrived,arrives),
    from,
    the,
    south
  ],
  [ sense(see,
          [ you_are(in,living_room),
            exits_are([east,south]),
            here_are(['player~1','videocamera~1'])
          ])
  ],
  timestamp(8,1764.9),
  did(goto(walk,west,L38,M38)),
  [ moved('floyd~1',kitchen,in,garden),
    [cap(subj('floyd~1')),person(go,goes),south]
  ],
  timestamp(7,1762.6),
  [ moved('floyd~1',dining_room,in,kitchen),
    the('floyd~1'),
    person(arrived,arrives),
    from,
    the,
    east
  ],
  timestamp(6,1751.5),
  [ moved('floyd~1',kitchen,in,dining_room),
    [cap(subj('floyd~1')),person(go,goes),east]
  ],
  timestamp(5,1720.6),
  [ moved('floyd~1',living_room,in,kitchen),
    the('floyd~1'),
    person(arrived,arrives),
    from,
    the,
    west
  ],
  timestamp(4,1710.2),
  [ moved('floyd~1',kitchen,in,living_room),
    [cap(subj('floyd~1')),person(go,goes),west]
  ],
  timestamp(3,1700.1),
  [ moved('floyd~1',pantry,in,kitchen),
    the('floyd~1'),
    person(arrived,arrives),
    from,
    the,
    north
  ],
  timestamp(2,1685.2),
  [ sense(see,
          [ you_are(in,kitchen),
            exits_are([north,south,east,west]),
            here_are(['cabinate~1','sink~1','table~1','player~1',screendoor])
          ])
  ],
  timestamp(1,1651.3),
  did(look),
  structure_label(mem('player~1')),
  timestamp(0,1650.3),
  inst('player~1'),
  name('player~1'),
  inherit(console,t),
  inherit(humanoid,t)
]
player~1@spatial>
````





