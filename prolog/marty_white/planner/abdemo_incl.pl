
%:- use_module(library(logicmoo_common)).

:- dynamic(abdemo_trace/1).

write_plan_len(A,B):- length(A,AL), length(B,BL),write_plan(AL,BL).
write_plan(HA,BA):- write('Plan: '), write(HA), write('-'), write(BA), write('    ').
/* Emulates the writenl(1) function */

%writeNoln(A) :-  write(A),!.
writeNoln(_).

writenl(_).

writeYesln(_).


when_tracing(N, G):- \+ abdemo_trace(N) -> true ; call(G).
 
is_sicstus:- \+ current_prolog_flag(version_data,swi(_,_,_,_)).

?- is_sicstus -> prolog_flag(single_var_warnings,_,off) ; true.

?- is_sicstus -> ensure_loaded(abdemo_incl_sicstus) ; ensure_loaded(abdemo_incl_swi).

:- style_check(-singleton).
/* Emulates the writenl(1) function */


