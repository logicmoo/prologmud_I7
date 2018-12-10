
%:- use_module(library(logicmoo_common)).

%writenl(P):- write(P),nl. 
%writeln(P):- write(P),nl. 

ticks(Z1):-  statistics(runtime,[Z1,_]).


init_gensym(_).

prolog_flag(F,Old,New):- ignore(current_prolog_flag(F,Old)),set_prolog_flag(F,New).

:- style_check(-singleton).
/* Emulates the writenl(1) function */


