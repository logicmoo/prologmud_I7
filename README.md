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
(base) root@gitlab:/opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog# su - tmptest
tmptest@gitlab:~$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.15)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?-  pack_install(prologmud_I7).
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Install prologmud_I7@1.2.111 from https://github.com/TeamSPoon/small_adventure_games.git Y/n? Y

Create directory for packages
   (1) * /home/tmptest/.local/share/swi-prolog/pack
   (2)   Cancel

Your choice? 1
% Cloning into '/home/tmptest/.local/share/swi-prolog/pack/small_adventure_games'...
% Checking out files:  90% (1313/1458)
% Checking out files:  91% (1327/1458)
% Checking out files:  92% (1342/1458)
% Checking out files:  93% (1356/1458)
% Checking out files:  94% (1371/1458)
% Checking out files:  95% (1386/1458)
% Checking out files:  96% (1400/1458)
% Checking out files:  97% (1415/1458)
% Checking out files:  98% (1429/1458)
% Checking out files:  99% (1444/1458)
Checking out files: 100% (1458/1458), done.
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Warning: Package depends on the following:
Warning:   "logicmoo_utils", provided by logicmoo_utils@1.2.111 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "predicate_streams", provided by predicate_streams@1.2.111 from http://github.com/TeamSPoon/predicate_streams.git

What do you wish to do
   (1) * Install proposed dependencies
   (2)   Only install requested package
   (3)   Cancel

Your choice? 1
% "small_adventure_games.git" was downloaded 1 times
% Cloning into '/home/tmptest/.local/share/swi-prolog/pack/logicmoo_utils'...
i logicmoo_utils@1.2.111    - Common predicates that are used throughout LogicMOO Software
% Updating index for library /home/tmptest/.local/share/swi-prolog/pack/logicmoo_utils/prolog/
% Cloning into '/home/tmptest/.local/share/swi-prolog/pack/predicate_streams'...
Warning: warning: redirecting to https://github.com/TeamSPoon/predicate_streams.git/
i predicate_streams@1.2.111 - Implement your own Abstract Predicate Streams
% Updating index for library /home/tmptest/.local/share/swi-prolog/pack/predicate_streams/prolog/
Package:                small_adventure_games
Title:                  PrologMUD I7 (NomicMU!) with small Adventure Games in Prolog using the CHAT80 Prolog natural language application
Installed version:      1.2.111
Author:                 Douglas R. Miles <logicmoo@gmail.com>, Marty White <lmw22@cornell.edu>, Fernando C.N. Pereira <unknown@unknown>, David H.D. Warren <unknown@unknown>, Jan Wielemaker <J.Wielemaker@vu.nl>
Maintainer:             Douglas R, Miles <logicmoo@gmail.com>
Home page:              https://github.com/TeamSPoon/prologmud_I7.git
Download URL:           https://github.com/TeamSPoon/prologmud_I7/archive/V*.zip
Requires:               logicmoo_utils
Activate pack "small_adventure_games" Y/n? Y
true.

?-
````

## Use the module

````
?- ensure_loaded(library(nomic_mu)).
````

### First time will do this ...
````
?- ensure_loaded(library(nomic_mu)).
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Install dictoo@1.2.111 from GIT at https://github.com/TeamSPoon/dictoo.git Y/n? Y
% Cloning into '/home/tmptest/.local/share/swi-prolog/pack/dictoo'...
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Warning: Package depends on the following:
Warning:   "gvar_syntax", provided by gvar_syntax@1.2.111 from https://github.com/TeamSPoon/gvar_syntax.git

What do you wish to do
   (1) * Install proposed dependencies
   (2)   Only install requested package
   (3)   Cancel

Your choice? 1
% "dictoo.git" was downloaded 10 times
% Cloning into '/home/tmptest/.local/share/swi-prolog/pack/gvar_syntax'...
i gvar_syntax@1.2.111       - Global Variable Syntax
Package:                dictoo
Title:                  Dict-like OO Syntax
Installed version:      1.2.111
Author:                 Douglas R. Miles <logicmoo@gmail.com>
Maintainer:             TeamSPoon <https://github.com/TeamSPoon/>
Packager:               TeamSPoon/LogicMoo <https://github.com/TeamSPoon/>
Home page:              https://github.com/TeamSPoon/dictoo
Download URL:           https://github.com/TeamSPoon/dictoo/releases/*.zip
Requires:               gvar_syntax
Activate pack "dictoo" Y/n? Y
Loading tt0_00022_cycl.qlf  ...
% 28,465,540 inferences, 11.850 CPU in 11.859 seconds (100% CPU, 2402080 Lips)
Loading ac_xnl_7166.qlf (this may take 60-120 seconds the very first time) ...
% 201,098,429 inferences, 77.354 CPU in 77.401 seconds (100% CPU, 2599732 Lips)
Loading clex_nldata.qlf ...
% 406,594 inferences, 0.417 CPU in 0.417 seconds (100% CPU, 974460 Lips)

You may start the server with:

 ?- srv_mu.

true.

[debug]  ?- halt.
````

Lets exit though ..


## Run it from scratch

Will be lots of inforational spam sorry.. [[skip past]]

````
tmptest@gitlab:~$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.15)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- ensure_loaded(library(nomic_mu)).
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
% Pack `dictoo' is already installed @1.2.111
Loading tt0_00022_cycl.qlf  ...
% 648 inferences, 0.194 CPU in 0.194 seconds (100% CPU, 3339 Lips)
Loading ac_xnl_7166.qlf (this may take 60-120 seconds the very first time) ...
% 752 inferences, 1.261 CPU in 1.261 seconds (100% CPU, 596 Lips)
Loading clex_nldata.qlf ...
% 391,074 inferences, 0.371 CPU in 0.372 seconds (100% CPU, 1052937 Lips)

You may start the server with:

 ?- srv_mu.

true.

[debug]  ?- srv_mu.

% adv_server(2666)
Server is starting on port 2666
%  Thread Status       Time    Stack use    allocated
% ---------------------------------------------------
%    main running    10.247      182,704      505,696
%      gc running     0.418          848      112,480
% mu_2666 running     0.000          912      112,480

% 'ORDERING TEST:~n'
%  unordered was {6,[before(start,finish),before(start,x),before(start,y),before(y,finish),before(x,z)],...(_48590)}

%  ordering is {8,[before(z,finish),before(x,finish),before(x,z),before(start,z),before(y,finish)],...(_49058)}

%  picked [start,x,y,z,finish]

%  picked [start,x,z,y,finish]

%  picked [start,y,x,z,finish]

% ' END ORDERING TEST~n'
% /home/tmptest/.local/share/swi-prolog/pack/small_adventure_games/prolog/nldata/clex_lexicon_user1.nldata:97543
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_15666)]), type_props(wrench, [inherit(shiny, t)]), ...(_15682)], the(cup), _15620).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_29372)]), type_props(wrench, [inherit(shiny, t)]), ...(_29388)], the(cabinate), _29326).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_43698)]), type_props(wrench, [inherit(shiny, t)]), ...(_43714)], the(cabinate), _43652).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_57670)]), type_props(wrench, [inherit(shiny, t)]), ...(_57686)], kitchen, _57624).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_71894)]), type_props(wrench, [inherit(shiny, t)]), ...(_71910)], the(plate), _71848).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_9588)]), type_props(wrench, [inherit(shiny, t)]), ...(_9604)], the(sink), _9542).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_24194)]), type_props(wrench, [inherit(shiny, t)]), ...(_24210)], the(sink), _24148).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_38530)]), type_props(wrench, [inherit(shiny, t)]), ...(_38546)], kitchen, _38484).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_53142)]), type_props(wrench, [inherit(shiny, t)]), ...(_53158)], the(lamp), _53096).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_67580)]), type_props(wrench, [inherit(shiny, t)]), ...(_67596)], the(table), _67534).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_11300)]), type_props(wrench, [inherit(shiny, t)]), ...(_11316)], the(table), _11254).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_25896)]), type_props(wrench, [inherit(shiny, t)]), ...(_25912)], kitchen, _25850).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_40896)]), type_props(wrench, [inherit(shiny, t)]), ...(_40912)], the(flour), _40850).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_55698)]), type_props(wrench, [inherit(shiny, t)]), ...(_55714)], the(bowl), _55652).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_71188)]), type_props(wrench, [inherit(shiny, t)]), ...(_71204)], the(bowl), _71142).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_12590)]), type_props(wrench, [inherit(shiny, t)]), ...(_12606)], the(box), _12544).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_28166)]), type_props(wrench, [inherit(shiny, t)]), ...(_28182)], the(box), _28120).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_43448)]), type_props(wrench, [inherit(shiny, t)]), ...(_43464)], the(table), _43402).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_59470)]), type_props(wrench, [inherit(shiny, t)]), ...(_59486)], the(table), _59424).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_74938)]), type_props(wrench, [inherit(shiny, t)]), ...(_74954)], the(table_leg), _74892).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_14600)]), type_props(wrench, [inherit(shiny, t)]), ...(_14616)], the(apple), _14554).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_30292)]), type_props(wrench, [inherit(shiny, t)]), ...(_30308)], the(crate), _30246).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_46866)]), type_props(wrench, [inherit(shiny, t)]), ...(_46882)], the(crate), _46820).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_62924)]), type_props(wrench, [inherit(shiny, t)]), ...(_62940)], kitchen, _62878).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_79440)]), type_props(wrench, [inherit(shiny, t)]), ...(_79456)], the(fireplace), _79394).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_16090)]), type_props(wrench, [inherit(shiny, t)]), ...(_16106)], living_room, _16044).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_32702)]), type_props(wrench, [inherit(shiny, t)]), ...(_32718)], the(videocamera), _32656).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_49042)]), type_props(wrench, [inherit(shiny, t)]), ...(_49058)], living_room, _48996).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_65950)]), type_props(wrench, [inherit(shiny, t)]), ...(_65966)], the(shovel), _65904).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_82472)]), type_props(wrench, [inherit(shiny, t)]), ...(_82488)], basement, _82426).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_18188)]), type_props(wrench, [inherit(shiny, t)]), ...(_18204)], the(mushroom), _18142).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_34786)]), type_props(wrench, [inherit(shiny, t)]), ...(_34802)], garden, _34740).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_52078)]), type_props(wrench, [inherit(shiny, t)]), ...(_52094)], the(fountain), _52032).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_68962)]), type_props(wrench, [inherit(shiny, t)]), ...(_68978)], garden, _68916).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_86448)]), type_props(wrench, [inherit(shiny, t)]), ...(_86464)], the(rock), _86402).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_103514)]), type_props(wrench, [inherit(shiny, t)]), ...(_103530)], garden, _103468).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_20450)]), type_props(wrench, [inherit(shiny, t)]), ...(_20466)], the(locker), _20404).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_37594)]), type_props(wrench, [inherit(shiny, t)]), ...(_37610)], pantry, _37548).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_55468)]), type_props(wrench, [inherit(shiny, t)]), ...(_55484)], the(shelf), _55422).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_72898)]), type_props(wrench, [inherit(shiny, t)]), ...(_72914)], pantry, _72852).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_91494)]), type_props(wrench, [inherit(shiny, t)]), ...(_91510)], the(wrench), _91448).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_109562)]), type_props(wrench, [inherit(shiny, t)]), ...(_109578)], floyd, _109516).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_22964)]), type_props(wrench, [inherit(shiny, t)]), ...(_22980)], the(coins), _22918).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_41152)]), type_props(wrench, [inherit(shiny, t)]), ...(_41168)], the(bag), _41106).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_60428)]), type_props(wrench, [inherit(shiny, t)]), ...(_60444)], the(bag), _60382).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_79018)]), type_props(wrench, [inherit(shiny, t)]), ...(_79034)], the(player), _78972).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_98492)]), type_props(wrench, [inherit(shiny, t)]), ...(_98508)], the(watch), _98446).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_117148)]), type_props(wrench, [inherit(shiny, t)]), ...(_117164)], the(player), _117102).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_38096)]), type_props(wrench, [inherit(shiny, t)]), ...(_38112)], the(player), _38050).
% create_1obj('~1', [type_props(broken_videocam, [can_be(switch, f), powered=f, inherit(videocamera, t)]), type_props(videocamera, [inherit(memorize, t), inherit(perceptq, t), inherit(memorize_perceptq, t), ...(_57014)]), type_props(wrench, [inherit(shiny, t)]), ...(_57030)], kitchen, _56968).

% iObjectList={34,['player~1','watch~1','bag~1','coins~1','wrench~1'],...(_60412)}
% create_object('player~1',[shape=player,inherit(player,t),nouns([player])])
% create_object('watch~1',[shape=watch,inherit(watch,t),nouns([watch])])
% create_object('bag~1',[shape=bag,inherit(bag,t),nouns([bag])])
% create_object('coins~1',[shape=coins,inherit(coins,t),nouns([coins])])
% create_object('wrench~1',[shape=wrench,inherit(wrench,t),nouns([wrench])])
% create_object('shelf~1',[shape=shelf,inherit(shelf,t),nouns([shelf])])
% create_object('locker~1',[shape=locker,inherit(locker,t),nouns([locker])])
% create_object('rock~1',[shape=rock,inherit(rock,t),nouns([rock])])
% create_object('fountain~1',[shape=fountain,inherit(fountain,t),nouns([fountain])])
% create_object('mushroom~1',[shape=mushroom,inherit(mushroom,t),nouns([mushroom])])
% create_object('shovel~1',[shape=shovel,inherit(shovel,t),nouns([shovel])])
% create_object('videocamera~1',[shape=videocamera,inherit(videocamera,t),nouns([videocamera])])
% create_object('fireplace~1',[shape=fireplace,inherit(fireplace,t),nouns([fireplace])])
% create_object('crate~1',[shape=crate,inherit(crate,t),nouns([crate])])
% create_object('apple~1',[shape=apple,inherit(apple,t),nouns([apple])])
% create_object('table_leg~1',[shape=table_leg,inherit(table_leg,t),nouns([table_leg])])
% create_object('table~1',[shape=(table),inherit(table,t),nouns([table])])
% create_object('box~1',[shape=box,inherit(box,t),nouns([box])])
% create_object('bowl~1',[shape=bowl,inherit(bowl,t),nouns([bowl])])
% create_object('flour~1',[shape=flour,inherit(flour,t),nouns([flour])])
% create_object('lamp~1',[shape=lamp,inherit(lamp,t),nouns([lamp])])
% create_object('sink~1',[shape=sink,inherit(sink,t),nouns([sink])])
% create_object('plate~1',[shape=plate,inherit(plate,t),nouns([plate])])
% create_object('cabinate~1',[shape=cabinate,inherit(cabinate,t),nouns([cabinate])])
% create_object('cup~1',[shape=cup,inherit(cup,t),nouns([cup])])
% create_object(floyd,[name='Floyd the robot',powered=f,inherit(autonomous,t),inherit(robot,t)])
% create_object(screendoor,[door_to(kitchen),door_to(garden),opened=f,inherit(door,t)])
% create_object(brklamp,[inherit(broken,t),name='possibly broken lamp',effect(switch(on),print_(_4448,"Switch is flipped")),effect(hit,[print_("Hit brklamp"),setprop($self,inherit(broken))]),inherit(lamp,t)])
% create_object(pantry,{6,[volume_capacity=1000,nouns(closet),nominals(kitchen),desc='You\'re in a dark kitchen pantry.',dark=t],...(_132008)})
% create_object(living_room,[inherit(place,t)])
% create_object(kitchen,[inherit(place,t),desc='cooking happens here'])
% create_object(garden,[inherit(place,t),cant_go($agent,up,'You lack the ability to fly.'),cant_go($agent,_4928,'The fence surrounding the garden is too tall and solid to pass.')])
% create_object(dining_room,[inherit(place,t)])
% create_object(basement,[inherit(place,t),desc='This is a very dark basement.',dark=t])=============================================
INIT STATE
=============================================
[ props(basement,
        [ volume_capacity = 10000,
          default_rel = in,
          has_rel(in,t),
          nouns([here,basement]),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret),t),
          inherited(place),
          desc = 'This is a very dark basement.',
          dark = t,
          co([ inherit(place,t),
               desc = 'This is a very dark basement.',
               dark = t
             ])
        ]),
  props(dining_room,
        [ volume_capacity = 10000,
          default_rel = in,
          has_rel(in,t),
          nouns([here,dining_room]),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret2),t),
          inherited(place),
          co([inherit(place,t)])
        ]),
  props(garden,
        [ volume_capacity = 10000,
          default_rel = in,
          has_rel(in,t),
          nouns([here,garden]),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret3),t),
          inherited(place),
          cant_go($agent,up,'You lack the ability to fly.'),
          co([ inherit(place,t),
               cant_go($agent,up,'You lack the ability to fly.'),
               cant_go($ agent,
                       up,
                       'The fence surrounding the garden is too tall and solid to pass.')
             ])
        ]),
  props(kitchen,
        [ volume_capacity = 10000,
          default_rel = in,
          has_rel(in,t),
          nouns([here,kitchen]),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret4),t),
          inherited(place),
          desc = 'cooking happens here',
          co([inherit(place,t),desc='cooking happens here'])
        ]),
  props(living_room,
        [ volume_capacity = 10000,
          default_rel = in,
          has_rel(in,t),
          nouns([here,living_room]),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret5),t),
          inherited(place),
          co([inherit(place,t)])
        ]),
  props(pantry,
        [ volume_capacity = 1000,
          nouns([closet,here,pantry]),
          nominals(kitchen),
          desc = 'You\'re in a dark kitchen pantry.',
          dark = t,
          default_rel = in,
          has_rel(in,t),
          adjs([locally]),
          can_be(move,f),
          can_be(take,f),
          has_rel(exit(Exit_Ret6),t),
          inherited(place),
          co([ volume_capacity = 1000,
               nouns(closet),
               nominals(kitchen),
               desc = 'You\'re in a dark kitchen pantry.',
               dark = t,
               inherit(place,t)
             ])
        ]),
  props(brklamp,
        [ name = 'definately broken',
          effect(switch(on),true),
          effect(switch(off),true),
          adjs([dented,broken,shiny,physical,fully_corporial,thinkable]),
          inherited(broken),
          effect(hit,
                 [print_("Hit brklamp"),setprop(brklamp,inherit(broken))]),
          nouns([lamp,light,brklamp]),
          powered = t,
          can_be(switch,t),
          nominals(brass),
          inherited(shiny),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          emitting(see,light),
          breaks_into = broken_lamp,
          inherited(lamp),
          co([ inherit(broken,t),
               name = 'possibly broken lamp',
               effect(switch(on),
                      print_(Print_Param,"Switch is flipped")),
               effect(hit,
                      [ print_("Hit brklamp"),
                        setprop($self,inherit(broken))
                      ]),
               inherit(lamp,t)
             ])
        ]),
  props(screendoor,
        [ door_to(kitchen),
          door_to(garden),
          opened = f,
          nouns([door,screendoor]),
          can_be(take,f),
          can_be(open,t),
          can_be(close,t),
          can_be(touch,t),
          can_be(examine,t),
          adjs([thinkable,fully_corporial]),
          class_desc(['kind is normally thinkable','kind is corporial']),
          inherited(thinkable),
          cleanliness = clean,
          inherited(fully_corporial),
          inherited(door),
          co([ door_to(kitchen),
               door_to(garden),
               opened = f,
               inherit(door,t)
             ])
        ]),
  props(floyd,
        [ name = 'Floyd the robot',
          powered = f,
          knows_verbs(eat,f),
          nouns([ autonomous,
                  robot,
                  character,
                  no_perceptq,
                  actor,
                  autoscan,
                  floyd
                ]),
          inherited(autonomous),
          emitting(see,light),
          mass = 200,
          adjs([ metallic,
                 shiny,
                 physical,
                 fully_corporial,
                 partly_noncorporial,
                 thinkable,
                 noncorporial
               ]),
          desc = 'Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.',
          can_be(switch,t),
          can_be(move,t),
          class_desc([ 'kind is an Movable Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'direct inheriters are completely noncorporial',
                       'kind is both partly corporial and non-corporial'
                     ]),
          inherited(object),
          inherited(shiny),
          has_rel(worn_by,t),
          has_rel(held_by,t),
          model_depth = 3,
          volume = 50,
          has_sense(see),
          inherited(no_perceptq),
          knows_verbs(examine,t),
          inherited(actor),
          inherited(autoscan),
          can_be(touch,t),
          can_be(examine,t),
          cleanliness = clean,
          inherited(fully_corporial),
          inherited(thinkable),
          ~(inherit(fully_corporial)),
          inherited(noncorporial),
          inherited(partly_noncorporial),
          inherited(character),
          effect(switch(on),setprop(floyd,powered=t)),
          effect(switch(off),setprop(floyd,powered=f)),
          inherited(robot),
          co([ name = 'Floyd the robot',
               powered = f,
               inherit(autonomous,t),
               inherit(robot,t)
             ])
        ]),
  memories(floyd,
           [ structure_label(mem(floyd)),
             timestamp(0,51.7),
             goals([]),
             goals_skipped([]),
             goals_satisfied([]),
             todo([look(floyd)]),
             inst(floyd)
           ]),
  props('cup~1',
        [ shape = cup,
          nouns([cup,container,'cup~1']),
          adjs([physical,fully_corporial,thinkable]),
          default_rel = in,
          opened = f,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          inherited(flask),
          inherited(cup),
          co([shape=cup,inherit(cup,t),nouns([cup])])
        ]),
  props('cabinate~1',
        [ shape = cabinate,
          nouns([cabinate,container,'cabinate~1']),
          default_rel = in,
          opened = f,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          adjs([untakeable,fully_corporial,physical,thinkable]),
          can_be(take,f),
          class_desc([ 'kind is an Immobile Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'kind is furnature'
                     ]),
          inherited(untakeable),
          can_be(touch,t),
          inherited(fully_corporial),
          has_rel(on,t),
          cleanliness = clean,
          inherited(surface),
          can_be(examine,t),
          inherited(thinkable),
          inherited(furnature),
          volume_capacity = 10,
          inherited(cabinate),
          co([shape=cabinate,inherit(cabinate,t),nouns([cabinate])])
        ]),
  props('plate~1',
        [ shape = plate,
          nouns([plate,'plate~1']),
          has_rel(on,t),
          default_rel = on,
          adjs([physical,fully_corporial,thinkable]),
          inherited(surface),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          volume_capacity = 2,
          breaks_into = shards,
          name = plate,
          inherited(plate),
          co([shape=plate,inherit(plate,t),nouns([plate])])
        ]),
  props('sink~1',
        [ shape = sink,
          nouns([sink,uncloseable,container,'sink~1']),
          cleanliness = dirty,
          opened = t,
          can_be(close,f),
          can_be(open,f),
          inherited(uncloseable),
          adjs([physical,untakeable,fully_corporial,thinkable]),
          default_rel = in,
          has_rel(in,t),
          inherited(container),
          can_be(move,t),
          class_desc([ 'kind is an Movable Object',
                       'kind is an Immobile Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'kind is furnature'
                     ]),
          inherited(object),
          inherited(flask),
          can_be(take,f),
          inherited(untakeable),
          can_be(touch,t),
          inherited(fully_corporial),
          has_rel(on,t),
          inherited(surface),
          can_be(examine,t),
          inherited(thinkable),
          inherited(furnature),
          volume_capacity = 5,
          inherited(sink),
          co([shape=sink,inherit(sink,t),nouns([sink])])
        ]),
  props('lamp~1',
        [ shape = lamp,
          nouns([lamp,light,'lamp~1']),
          name = 'shiny brass lamp',
          powered = t,
          can_be(switch,t),
          nominals(brass),
          adjs([shiny,physical,fully_corporial,thinkable]),
          inherited(shiny),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          emitting(see,light),
          effect(switch(on),setprop('lamp~1',emitting(see,light))),
          effect(switch(off),delprop('lamp~1',emitting(see,light))),
          breaks_into = broken_lamp,
          inherited(lamp),
          co([shape=lamp,inherit(lamp,t),nouns([lamp])])
        ]),
  props('flour~1',
        [ shape = flour,
          nouns([flour,food,'flour~1']),
          can_be(eat,t),
          adjs([physical,fully_corporial,thinkable,measurable]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          inherited(food),
          ammount = some,
          inherited(measurable),
          inherited(flour),
          co([shape=flour,inherit(flour,t),nouns([flour])])
        ]),
  props('bowl~1',
        [ shape = bowl,
          nouns([bowl,uncloseable,container,'bowl~1']),
          opened = t,
          can_be(close,f),
          can_be(open,f),
          inherited(uncloseable),
          adjs([physical,fully_corporial,thinkable]),
          default_rel = in,
          has_rel(in,t),
          inherited(container),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          inherited(flask),
          volume_capacity = 2,
          breaks_into = shards,
          name = 'porcelain bowl',
          desc = 'This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.',
          inherited(bowl),
          co([shape=bowl,inherit(bowl,t),nouns([bowl])])
        ]),
  props('box~1',
        [ shape = box,
          nouns([box,container,'box~1',cardboard,paper]),
          volume_capacity = 11,
          default_rel = in,
          opened = f,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          adjs([physical,fully_corporial,thinkable]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          can_be(burn,t),
          inherited(paper),
          inherited(cardboard),
          inherited(box),
          co([shape=box,inherit(box,t),nouns([box])])
        ]),
  props('table~1',
        [ shape = table,
          has_rel(on,t),
          cleanliness = clean,
          inherited(surface),
          adjs(physical),
          default_rel = on,
          inherited(table),
          nouns([table]),
          co([shape=(table),inherit(table,t),nouns([table])])
        ]),
  props('table_leg~1',
        [ shape = table_leg,
          inherited(table_leg),
          nouns([table_leg]),
          co([shape=table_leg,inherit(table_leg,t),nouns([table_leg])])
        ]),
  props('apple~1',
        [ shape = apple,
          inherited(apple),
          nouns([apple]),
          co([shape=apple,inherit(apple,t),nouns([apple])])
        ]),
  props('crate~1',
        [ shape = crate,
          nouns([crate,container,'crate~1',wooden]),
          default_rel = in,
          opened = f,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          adjs([physical,fully_corporial,thinkable]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          volume_capacity = 13,
          breaks_into = splinters,
          can_be(burn,t),
          inherited(wooden),
          inherited(crate),
          co([shape=crate,inherit(crate,t),nouns([crate])])
        ]),
  props('fireplace~1',
        [ shape = fireplace,
          nouns([fireplace,uncloseable,container,'fireplace~1']),
          has_rel(on,f),
          has_rel(over,t),
          opened = t,
          can_be(close,f),
          can_be(open,f),
          default_rel = in,
          has_rel(in,t),
          inherited(container),
          inherited(uncloseable),
          volume_capacity = 20,
          adjs([untakeable,fully_corporial,physical,thinkable]),
          can_be(take,f),
          class_desc([ 'kind is an Immobile Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'kind is furnature'
                     ]),
          inherited(untakeable),
          can_be(touch,t),
          inherited(fully_corporial),
          cleanliness = clean,
          inherited(surface),
          can_be(examine,t),
          inherited(thinkable),
          inherited(furnature),
          inherited(fireplace),
          co([shape=fireplace,inherit(fireplace,t),nouns([fireplace])])
        ]),
  props('videocamera~1',
        [ shape = videocamera,
          nouns([videocamera,memorize_perceptq]),
          inherited(memorize_perceptq),
          can_be(switch,t),
          effect(switch(on),setprop('videocamera~1',powered=t)),
          effect(switch(off),setprop('videocamera~1',powered=f)),
          powered = t,
          has_sense(see),
          breaks_into = broken_videocam,
          inherited(videocamera),
          co([ shape = videocamera,
               inherit(videocamera,t),
               nouns([videocamera])
             ])
        ]),
  memories('videocamera~1',
           [ structure_label(mem('videocamera~1')),
             timestamp(0,51.6),
             goals([]),
             goals_skipped([]),
             goals_satisfied([]),
             todo([look('videocamera~1')]),
             inst('videocamera~1')
           ]),
  perceptq('videocamera~1',[]),
  props('shovel~1',
        [ shape = shovel,
          inherited(shovel),
          nouns([shovel]),
          co([shape=shovel,inherit(shovel,t),nouns([shovel])])
        ]),
  props('mushroom~1',
        [ shape = mushroom,
          name = 'speckled mushroom',
          nouns([food,'mushroom~1',mushroom,fungus,toadstool]),
          adjs([physical,fully_corporial,thinkable,measurable,speckled]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          ammount = some,
          inherited(measurable),
          inherited(food),
          initial('A speckled mushroom grows out of the sodden earth, on a long stalk.'),
          desc = 'The mushroom is capped with blotches, and you aren\'t at all sure it\'s not a toadstool.',
          can_be(eat,t),
          before(eat,(random100=<30,die('It was poisoned!');'yuck!')),
          after(take,
                initial ',' 'You pick the mushroom, neatly cleaving its thin stalk.'),
          inherited(mushroom),
          co([shape=mushroom,inherit(mushroom,t),nouns([mushroom])])
        ]),
  props('fountain~1',
        [ shape = fountain,
          nouns([fountain,here,'fountain~1',sink,uncloseable,container]),
          volume_capacity = 150,
          adjs([locally,physical,untakeable,fully_corporial,thinkable]),
          can_be(move,f),
          has_rel(exit(Exit_Ret7),t),
          inherited(place),
          cleanliness = dirty,
          opened = t,
          can_be(close,f),
          can_be(open,f),
          inherited(uncloseable),
          default_rel = in,
          has_rel(in,t),
          inherited(container),
          class_desc([ 'kind is an Movable Object',
                       'kind is an Immobile Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'kind is furnature'
                     ]),
          inherited(object),
          inherited(flask),
          can_be(take,f),
          inherited(untakeable),
          can_be(touch,t),
          inherited(fully_corporial),
          has_rel(on,t),
          inherited(surface),
          can_be(examine,t),
          inherited(thinkable),
          inherited(furnature),
          inherited(sink),
          inherited(fountain),
          co([shape=fountain,inherit(fountain,t),nouns([fountain])])
        ]),
  props('rock~1',
        [ shape = rock,
          inherited(rock),
          nouns([rock]),
          co([shape=rock,inherit(rock,t),nouns([rock])])
        ]),
  props('locker~1',
        [ shape = locker,
          nouns([locker,container,'locker~1',metal]),
          default_rel = in,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          adjs([physical,fully_corporial,thinkable]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          volume_capacity = 13,
          can_be(burn,f),
          inherited(metal),
          opened = f,
          inherited(locker),
          co([shape=locker,inherit(locker,t),nouns([locker])])
        ]),
  props('shelf~1',
        [ shape = shelf,
          adjs([physical,untakeable,fully_corporial,thinkable]),
          can_be(take,f),
          class_desc([ 'kind is an Immobile Object',
                       'kind is corporial',
                       'kind is normally thinkable',
                       'kind is furnature'
                     ]),
          inherited(untakeable),
          can_be(touch,t),
          inherited(fully_corporial),
          has_rel(on,t),
          default_rel = on,
          cleanliness = clean,
          inherited(surface),
          can_be(examine,t),
          nouns(['shelf~1',shelf]),
          inherited(thinkable),
          inherited(furnature),
          inherited(shelf),
          co([shape=shelf,inherit(shelf,t),nouns([shelf])])
        ]),
  props('wrench~1',
        [ shape = wrench,
          nouns([wrench,'wrench~1']),
          adjs([shiny,physical,thinkable,fully_corporial]),
          can_be(move,t),
          class_desc([ 'kind is an Movable Object',
                       'kind is normally thinkable',
                       'kind is corporial'
                     ]),
          inherited(object),
          can_be(touch,t),
          can_be(examine,t),
          inherited(thinkable),
          cleanliness = clean,
          inherited(fully_corporial),
          inherited(shiny),
          inherited(wrench),
          co([shape=wrench,inherit(wrench,t),nouns([wrench])])
        ]),
  props('coins~1',
        [ shape = coins,
          nouns([coins,'coins~1']),
          adjs([shiny,physical,thinkable,fully_corporial,measurable]),
          can_be(move,t),
          class_desc([ 'kind is an Movable Object',
                       'kind is normally thinkable',
                       'kind is corporial'
                     ]),
          inherited(object),
          can_be(touch,t),
          can_be(examine,t),
          inherited(thinkable),
          cleanliness = clean,
          inherited(fully_corporial),
          inherited(shiny),
          ammount = some,
          inherited(measurable),
          inherited(coins),
          co([shape=coins,inherit(coins,t),nouns([coins])])
        ]),
  props('bag~1',
        [ shape = bag,
          nouns([bag,container,'bag~1']),
          volume_capacity = 10,
          default_rel = in,
          opened = f,
          can_be(open,t),
          has_rel(in,t),
          inherited(container),
          adjs([physical,fully_corporial,thinkable]),
          can_be(move,t),
          can_be(touch,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'kind is an Movable Object'
                     ]),
          inherited(fully_corporial),
          can_be(examine,t),
          inherited(thinkable),
          inherited(object),
          inherited(bag),
          co([shape=bag,inherit(bag,t),nouns([bag])])
        ]),
  props('watch~1',
        [ shape = watch,
          inherited(watch),
          nouns([watch]),
          co([shape=watch,inherit(watch,t),nouns([watch])])
        ]),
  props('player~1',
        [ shape = player,
          nouns([ player,
                  humanoid,
                  character,
                  no_perceptq,
                  actor,
                  autoscan,
                  'player~1'
                ]),
          name = 'player~1',
          look_depth = 2,
          user_mode = 2,
          access_level = admin,
          adjs([ physical,
                 fully_corporial,
                 partly_noncorporial,
                 thinkable,
                 noncorporial
               ]),
          nominals([console]),
          inherited(console),
          knows_verbs(eat,t),
          has_rel(worn_by,t),
          has_rel(held_by,t),
          model_depth = 3,
          mass = 50,
          volume = 50,
          has_sense(see),
          inherited(no_perceptq),
          knows_verbs(examine,t),
          inherited(actor),
          inherited(autoscan),
          can_be(touch,t),
          can_be(examine,t),
          cleanliness = clean,
          class_desc([ 'kind is corporial',
                       'kind is normally thinkable',
                       'direct inheriters are completely noncorporial',
                       'kind is both partly corporial and non-corporial'
                     ]),
          inherited(fully_corporial),
          inherited(thinkable),
          ~(inherit(fully_corporial)),
          inherited(noncorporial),
          inherited(partly_noncorporial),
          inherited(character),
          can(switch(off),f),
          powered = t,
          inherited(humanoid),
          inherited(player),
          co([shape=player,inherit(player,t),nouns([player])])
        ]),
  memories('player~1',
           [ structure_label(mem('player~1')),
             timestamp(0,51.5),
             goals([]),
             goals_skipped([]),
             goals_satisfied([]),
             todo([look('player~1')]),
             inst('player~1')
           ]),
  h(in,'player~1',kitchen),
  h(worn_by,'watch~1','player~1'),
  h(held_by,'bag~1','player~1'),
  h(in,'coins~1','bag~1'),
  h(held_by,'wrench~1',floyd),
  h(in,'shelf~1',pantry),
  h(in,'locker~1',pantry),
  h(in,'rock~1',garden),
  h(in,'fountain~1',garden),
  h(in,'mushroom~1',garden),
  h(in,'shovel~1',basement),
  h(in,'videocamera~1',living_room),
  h(in,'fireplace~1',living_room),
  h(in,'crate~1',kitchen),
  h(in,'apple~1','crate~1'),
  h(reverse(on),'table~1','table_leg~1'),
  h(on,'box~1','table~1'),
  h(in,'bowl~1','box~1'),
  h(in,'flour~1','bowl~1'),
  h(in,'table~1',kitchen),
  h(on,'lamp~1','table~1'),
  h(in,'sink~1',kitchen),
  h(in,'plate~1','sink~1'),
  h(in,'cabinate~1',kitchen),
  h(in,'cup~1','cabinate~1'),
  type_props(broken_videocam,
             [can_be(switch,f),powered=f,inherit(videocamera,t)]),
  type_props(videocamera,
             [ inherit(memorize,t),
               inherit(perceptq,t),
               inherit(memorize_perceptq,t),
               can_be(switch,t),
               effect(switch(on),setprop($self,powered=t)),
               effect(switch(off),setprop($self,powered=f)),
               powered = t,
               has_sense(see),
               breaks_into = broken_videocam
             ]),
  type_props(wrench,[inherit(shiny,t)]),
  type_props(table,[inherit(surface,t),adjs(physical),default_rel=on]),
  type_props(shelf,
             [inherit(surface,t),adjs(physical),inherit(furnature,t)]),
  type_props(surface,
             [ has_rel(on,t),
               default_rel = on,
               adjs(physical),
               cleanliness = clean
             ]),
  type_props(broken_lamp,
             [ name = 'dented brass lamp',
               nouns(light),
               nominals(brass),
               adjs(dented),
               can_be(switch,t),
               effect(switch(on),true),
               effect(switch(off),true)
             ]),
  type_props(lamp,
             [ name = 'shiny brass lamp',
               powered = t,
               can_be(switch,t),
               nouns(light),
               nominals(brass),
               inherit(shiny,t),
               inherit(object,t),
               emitting(see,light),
               effect(switch(on),setprop($self,emitting(see,light))),
               effect(switch(off),delprop($self,emitting(see,light))),
               breaks_into = broken_lamp
             ]),
  type_props(flour,[inherit(food,t),inherit(measurable,t)]),
  type_props(coins,[inherit(shiny,t),inherit(measurable,t)]),
  type_props(shiny,
             [ adjs($class),
               inherit(object,t),
               inherit(fully_corporial,t)
             ]),
  type_props(measurable,[adjs($class),ammount=some]),
  type_props(fountain,
             [volume_capacity=150,inherit(place,t),inherit(sink,t)]),
  type_props(cabinate,
             [ inherit(container,t),
               inherit(furnature,t),
               volume_capacity = 10
             ]),
  type_props(uncloseable,
             [ opened = t,
               can_be(close,f),
               can_be(open,f),
               inherit(container,t)
             ]),
  type_props(sink,
             [ cleanliness = dirty,
               inherit(uncloseable,t),
               inherit(flask,t),
               inherit(furnature,t),
               volume_capacity = 5
             ]),
  type_props(paper,[can_be(burn,t)]),
  type_props(cardboard,[inherit(paper,t)]),
  type_props(metal,[can_be(burn,f)]),
  type_props(wooden,[breaks_into=splinters,can_be(burn,t)]),
  type_props(locker,
             [ inherit(container,t),
               inherit(object,t),
               volume_capacity = 13,
               inherit(metal,t),
               opened = f
             ]),
  type_props(crate,
             [ inherit(container,t),
               inherit(object,t),
               volume_capacity = 13,
               inherit(wooden,t),
               opened = t
             ]),
  type_props(box,
             [ opened = f,
               volume_capacity = 11,
               inherit(container,t),
               inherit(object,t),
               inherit(cardboard,t)
             ]),
  type_props(fireplace,
             [ has_rel(on,f),
               has_rel(over,t),
               inherit(uncloseable,t),
               volume_capacity = 20,
               inherit(furnature,t)
             ]),
  type_props(plate,
             [ inherit(surface,t),
               inherit(object,t),
               volume_capacity = 2,
               breaks_into = shards,
               cleanliness = dirty,
               name = $ class
             ]),
  type_props(bowl,
             [ inherit(uncloseable,t),
               inherit(flask,t),
               volume_capacity = 2,
               breaks_into = shards,
               cleanliness = dirty,
               name = 'porcelain bowl',
               desc = 'This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.'
             ]),
  type_props(flask,
             [adjs(physical),inherit(container,t),inherit(object,t)]),
  type_props(cup,[inherit(flask,t)]),
  type_props(bag,
             [ volume_capacity = 10,
               inherit(container,t),
               inherit(object,t)
             ]),
  type_props(container,
             [default_rel=in,opened=f,can_be(open,t),has_rel(in,t)]),
  type_props(place,
             [ volume_capacity = 10000,
               default_rel = in,
               has_rel(in,t),
               nouns([here,$self]),
               adjs([locally]),
               can_be(move,f),
               can_be(take,f),
               has_rel(exit(Exit_Ret8),t)
             ]),
  type_props(natural_force,
             [ knows_verbs(eat,f),
               can_be(touch,f),
               has_rel(held_by,f),
               has_rel(worn_by,f),
               has_sense(see),
               inherit(no_perceptq,t),
               inherit(noncorporial,t),
               inherit(actor,t)
             ]),
  type_props(robot,
             [ knows_verbs(eat,f),
               inherit(autonomous,t),
               emitting(see,light),
               volume = 50,
               mass = 200,
               nouns([robot]),
               adjs([metallic]),
               desc = 'Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.',
               can_be(switch,t),
               inherit(memorize,t),
               nouns($class),
               inherit(shiny,t),
               inherit(character,t),
               powered = t,
               effect(switch(on),setprop($self,powered=t)),
               effect(switch(off),setprop($self,powered=f))
             ]),
  type_props(actor,
             [knows_verbs(examine,t),inherit(partly_noncorporial,t)]),
  type_props(character,
             [ has_rel(worn_by,t),
               has_rel(held_by,t),
               model_depth = 3,
               mass = 50,
               volume = 50,
               has_sense(see),
               inherit(no_perceptq,t),
               inherit(memorize,t),
               inherit(actor,t),
               inherit(autoscan,t),
               inherit(partly_noncorporial,t)
             ]),
  type_props(autonomous,[inherit(autoscan,t)]),
  type_props(humanoid,
             [ knows_verbs(eat,t),
               volume = 50,
               mass = 50,
               inherit(character,t),
               inherit(memorize,t),
               can(switch(off),f),
               powered = t
             ]),
  type_props(console,
             [adjs(physical),nominals([console]),nouns([player])]),
  type_props(player,
             [ name = $ self,
               model_depth = 3,
               look_depth = 2,
               user_mode = 2,
               access_level = admin,
               inherit(console,t),
               inherit(humanoid,t)
             ]),
  type_props(telnet,[adjs([remote]),inherit(player,t),nouns([player])]),
  type_props(furnature,
             [ can_be(examine,t),
               inherit(untakeable,t),
               inherit(fully_corporial,t),
               inherit(surface,t),
               inherit(thinkable,t),
               adjs(physical),
               class_desc(['kind is furnature'])
             ]),
  type_props(untakeable,
             [ adjs($class),
               can_be(take,f),
               class_desc(['kind is an Immobile Object'])
             ]),
  type_props(object,
             [ can_be(examine,t),
               adjs(physical),
               can_be(move,t),
               inherit(fully_corporial,t),
               inherit(thinkable,t),
               class_desc(['kind is an Movable Object'])
             ]),
  type_props(fully_corporial,
             [ can_be(touch,t),
               can_be(examine,t),
               inherit(thinkable,t),
               cleanliness = clean,
               adjs($class),
               class_desc(['kind is corporial'])
             ]),
  type_props(partly_noncorporial,
             [ inherit(fully_corporial,t),
               adjs($class),
               inherit(noncorporial,t),
               class_desc([ 'kind is both partly corporial and non-corporial'
                          ])
             ]),
  type_props(only_conceptual,
             [ adjs($class),
               inherit(noncorporial,t),
               inherit(thinkable,t),
               class_desc(['kind is only conceptual'])
             ]),
  type_props(noncorporial,
             [ can_be(examine,f),
               can_be(touch,f),
               inherit(thinkable,t),
               adjs($class),
               ~(inherit(fully_corporial)),
               class_desc([ 'direct inheriters are completely noncorporial'
                          ])
             ]),
  type_props(thinkable,
             [ can_be(examine,t),
               nouns($self),
               adjs($class),
               class_desc(['kind is normally thinkable'])
             ]),
  type_props(unthinkable,
             [ can_be(examine,f),
               adjs($class),
               class_desc(['kind is normally unthinkable'])
             ]),
  type_props(door,
             [ can_be(take,f),
               can_be(open,t),
               can_be(close,t),
               opened = t,
               nouns($class),
               inherit(fully_corporial,t),
               can_be(take,f),
               can_be(open,t),
               can_be(close,t),
               opened = t,
               nouns(door),
               inherit(fully_corporial,t)
             ]),
  type_props(mushroom,
             [ name = 'speckled mushroom',
               inherit(food,t),
               nouns([mushroom,fungus,toadstool]),
               adjs([speckled]),
               initial('A speckled mushroom grows out of the sodden earth, on a long stalk.'),
               desc = 'The mushroom is capped with blotches, and you aren\'t at all sure it\'s not a toadstool.',
               can_be(eat,t),
               before(eat,(random100=<30,die('It was poisoned!');'yuck!')),
               after(take,
                     initial ',' 'You pick the mushroom, neatly cleaving its thin stalk.')
             ]),
  type_props(broken,
             [ name = 'definately broken',
               effect(switch(on),true),
               effect(switch(off),true),
               can_be(switch,t),
               adjs([dented]),
               adjs($class)
             ]),
  type_props(food,
             [can_be(eat,t),inherit(object,t),inherit(measurable,t)]),
  h(in,brklamp,garden),
  h(in,screendoor,garden),
  h(in,screendoor,kitchen),
  h(exit(west),kitchen,living_room),
  h(exit(south),living_room,kitchen),
  h(exit(east),living_room,dining_room),
  h(exit(north),dining_room,living_room),
  h(exit(west),dining_room,kitchen),
  h(exit(east),kitchen,dining_room),
  h(exit(north),garden,kitchen),
  h(exit(south),kitchen,garden),
  h(exit(up),basement,pantry),
  h(exit(down),pantry,basement),
  h(exit(north),kitchen,pantry),
  h(exit(south),pantry,kitchen),
  h(in,floyd,pantry),
  structure_label(istate)
]
=============================================
Welcome to Marty's Prolog Adventure Prototype
=============================================

% player~1 @ kitchen: already about todo: look(player~1)

% aXiom(look('player~1'))
 {{ sub__examine('player~1',see,in,kitchen,3) }}

 {{ percept('player~1',see,3,props(kitchen,[volume_capacity=10000,has_rel(in,t),has_rel(exit(D2),t),desc='cooking happens here'])) }}
Player~1 is in kitchen and sees: crate, table, sink, cabinate and screendoor.
Exits in kitchen are: west, east, south and north.


 {{ percept('player~1',see,2,props('crate~1',[shape=crate,opened=f,has_rel(in,t)])) }}
'...verbose...'('crate is closed from seeing In')

 {{ percept('player~1',see,2,props('table~1',[shape=(table),has_rel(on,t)])) }}
Player~1 see on table: box and lamp.

 {{ percept('player~1',see,2,props('sink~1',[shape=sink,opened=t,has_rel(in,t),has_rel(on,t)])) }}
Player~1 see in sink: plate.
'...verbose...'('nothing on sink')

 {{ percept('player~1',see,2,props('cabinate~1',[shape=cabinate,opened=f,has_rel(in,t),has_rel(on,t)])) }}
'...verbose...'('cabinate is closed from seeing In')
'...verbose...'('nothing on cabinate')

 {{ percept('player~1',see,2,props(screendoor,[opened=f])) }}

'...verbose...'('percept(''player~1'',see,1,props(''box~1'',[shape=box]))')
'...verbose...'('box is closed from seeing In')

'...verbose...'('percept(''player~1'',see,1,props(''lamp~1'',[shape=lamp]))')

'...verbose...'('percept(''player~1'',see,1,props(''plate~1'',[shape=plate]))')
Player~1 see on plate: <nothing>
player~1@spatial>
````

## Do stuff

````
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
````
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

