
%:- use_module(library(logicmoo_common)).

%writenl(P):- write(P),nl. 
%writeln(P):- write(P),nl. 

ticks(Z1):-  statistics(runtime,[Z1,_]).


init_gensym(_).

prolog_flag(F,Old,New):- ignore(current_prolog_flag(F,Old)),set_prolog_flag(F,New).

:- style_check(-singleton).
/* Emulates the writenl(1) function */


  
system:clause_w_names(Head,Body,ClauseRef,file=line_of(Line,File),Vs):-   
  clause(Head,Body,ClauseRef),
  must_det_l(((
  clause(CHead,CBody,ClauseRef),
  term_variables(CHead+CBody,LocalVars),
  (prolog_clause:clause_info(ClauseRef, File, _TermPos, _,[variable_names(Vs)]) 
  -> 
  (  clause(CHead,CBody,ClauseRef),
     term_variables(CHead+CBody,LocalVars),
     term_variables(Vs,ClauseVars),
     ClauseVars = LocalVars
  ) 
  ;
  (
     Vs= [clauseVars=LocalVars]
  )),
  (_:Head)+Body = CHead+CBody,
  ignore(clause_property(ClauseRef, line_count(Line))),
  ignore(clause_property(ClauseRef, file(File)))))),
  dmsg(clause_w_names(Head,Body,ClauseRef,file=line_of(Line,File),Vs)).

