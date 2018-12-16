:- export(ect/0).
ect:- ec_load('examples/FrankEtAl2003/Story1.e').



%predicate, option range load fluent event noninertial xor completion
verbatum_functor(function). verbatum_functor(event). 
verbatum_functor(predicate). verbatum_functor(fluent).

builtin_pred_ef(initiates).
builtin_pred_ef(terminates).
builtin_pred_ef(releases).

builtin_pred(EF):- builtin_pred_ef(EF).
builtin_pred(holds_at).
builtin_pred(happens).
builtin_pred(declipped).
builtin_pred(clipped).
builtin_pred(before).
builtin_pred(after).
builtin_pred(sort).
builtin_pred(initially).

%special_directive('!').
%special_directive('neg').

decl_arg_sorts(fluent).
decl_arg_sorts(event).
decl_arg_sorts(predicate).
decl_arg_sorts(function).

% builtin_pred(releasedAt).

verbatum_functor(executable).
decl_arg_sorts(executable).


%process_ec(X):- ec_to_ax(X,Y),X\=@=Y,process_ec(Y).
process_ec_ax(ec_option(X,Y)):- assert(etmp:ec_option(X,Y)),dmsg(red(assert(ec_option(X,Y)))).
process_ec_ax(axiom(X,Y)):- s_l(S,L),nb_current('$variable_names',Vs),!,process_ec(axiom(X,Y,lsv(L:S,Vs))).
process_ec_ax(P):- assert(P),nop(dmsg(green(P))).

%xfr_body(true,[]).

ec_to_ax(X,X):- assertion(compound(X)),fail.
ec_to_ax(etmp:ec_option(X,Y),ec_option(X,Y)):-!.
ec_to_ax(Compound=Value, Term):- compound(Compound), append_term(Compound, Value, Term0), e_to_ec(Term0, Term).

/*
ec_to_ax(SS,O):- s_l(S,L),O=(etmp:ec_option(SS,L:S)),ground(SS),!.
ec_to_ax(SS,O):- s_l(S,L),O=(etmp:ec_option(SS,L:S+X)), nb_current('$variable_names',X),!.

*/


ec_to_ax(P,axiom(P,[])):- P =..[Initiates,E,H,T], builtin_pred_ef(Initiates),!.
% ec_to_ax(P:-B,axiom(P,[])):- P =..[Initiates,E,H,T], builtin_pred_ef(Initiates),!.

ec_to_ax(option(N,V),O):- retractall(etmp:ec_option(N,_)),ec_to_ax(ec_option(N,V),O).
ec_to_ax(holds_at(N,AT),O):- AT==0, ec_to_ax(initially(N),O).
ec_to_ax(event(P),O):- compound_name_arity(P,F,A),compound_name_arity(PP,F,A), ec_to_ax(executable(PP),O).
ec_to_ax(P,P):- functor(P,F,1),decl_arg_sorts(F),!.
ec_to_ax(P,P):- functor(P,F,_),is_special_macro(F),!.

ec_to_ax(ec_option(X,Y),ec_option(X,Y)):-!.
ec_to_ax(axiom(X,Y),axiom(X,Y)):-!.

ec_to_ax(isa(F, W), abducable(isa(F, W),[])):-!.
ec_to_ax(subsort(F, W), abducable(subsort(F, W),[])):-!.
%ec_to_ax(subsort(X,Y),subsort(X,Y)):-!.
ec_to_ax(range(X,Y,Z),range(X,Y,Z)):-!.

ec_to_ax(happens(F, T1, T2), O):- T1==T2,!, ec_to_ax(happens(F, T1), O).
ec_to_ax(happens(F, W), axiom(happens(F, W),[])):-!.
ec_to_ax(P,O):- P=..[C,E],C\==initially, !,ec_to_ax(isa(E,C),O).

ec_to_ax(initially(F), axiom(initially(F),[])):-!.
ec_to_ax(holds_at(F, W), axiom(holds_at(F, W),[])):-!.
ec_to_ax((PRE->POST),axiom(POST,List)):- /* functor(POST,F,2),builtin_pred(F),*/ conjuncts_to_list(PRE,List).
ec_to_ax((PRE),axiom(POST,[])):- /* functor(POST,F,2),builtin_pred(F),*/ ec_to_ax(PRE,POST).
ec_to_ax(O,O).


