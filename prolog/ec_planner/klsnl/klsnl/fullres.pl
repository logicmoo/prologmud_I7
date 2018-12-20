/* Predicates for collecting statistics */

collect_statistics((CPUTime,Inferences,LocalUsed,GlobalUsed,TrailUsed)):-
  statistics(cputime,CPUTime),
  statistics(inferences,Inferences),
  statistics(localused,LocalUsed),
  statistics(globalused,GlobalUsed),
  statistics(trailused,TrailUsed).

calculate_statistics((CPUTime1,Inferences1,LocalUsed1,GlobalUsed1,TrailUsed1),
		     (CPUTime2,Inferences2,LocalUsed2,GlobalUsed2,TrailUsed2)):-
  write('\n\nStatistics\n\n'),
  CPUTime is CPUTime2 - CPUTime1,
  writef('CPUTime:%w\n',[CPUTime]),
  Inferences is Inferences2 - Inferences1,
  writef('Inferences:%w\n',[Inferences]),
  LocalUsed is LocalUsed2 - LocalUsed1,
  writef('Local Stack:%t\n',[LocalUsed]),
  GlobalUsed is GlobalUsed2 - GlobalUsed1,
  writef('Global Stack:%t\n',[GlobalUsed]),
  TrailUsed is TrailUsed2 - TrailUsed1,
  writef('Trail Stack:%t\n',[TrailUsed]).

/* Predicate that invokes the other predicates that will perform resolution */
     
otres(Rules):-
  clear,
  collect_statistics(X),
  otres_allres(Rules,_),
  collect_statistics(Y),
  calculate_statistics(X,Y),
  newrulecount(N),
  Counter is N - 1,
  writef('\nNumber of clauses generated: %t\n',[Counter]).

/************************************************************/
/* First call to the predicates that perform resolution.    */
/* It takes a formula, transform it into the snf, set the   */
/* initial literal to be unique, number and separate rules, */
/* generate the SNL clauses and call modal resolution.      */
/************************************************************/

otres_allres(Rules,AllNewRules):-
   startrulecount(_),
   snff(Rules,SNFRules),
   check_initial(SNFRules,NewSNFRules),
   number_rules(NewSNFRules,[],NumberedSNF),
   separate_and_format_rules(NumberedSNF,ERules,ERulePaths,OtherRules),
   generate_snl(OtherRules,NOtherRules),
   simplify_lists(NOtherRules,(Initial,Literal,Modal,Temporal)),
   my_write('\nSet of clauses after translation to SNF :\n'),
   write_ruleset((Initial,Literal,Modal,Temporal)),
   mres(Literal,Modal,Initial,NewerLiteral,NewerModal),
   write('\notres_allres (1) ==> Usable rules are \n'),
   write_otter_rules(NewerLiteral),nl,
   write('\notres_allres (1) ==> Rules in the SoS are \n'),
   write_otter_rules(Temporal),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewerModal,Temporal),ERulePaths,ERules,AllNewRules).

/* predicate called after temporal resolution takes place */

otres_allres(OldRules,NewRules,ERulePaths,ERules,AllNewRules):-
   number_rules(NewRules,[],NumberedRules),
   simplify(NumberedRules,SimpleRules),
   split_temp_lit_rules(SimpleRules,NewLiteral,NewTemporal),
   my_write('otres_allres (2) ==> New literal rules are \n'),write_ruleset(NewLiteral),
   my_write('otres_allres (2) ==> New temporal rules are \n'),write_ruleset(NewTemporal),
   step_resolution(NewLiteral,NewTemporal,OldRules,ERules,ERulePaths,AllNewRules).

/************************************************************************************/
/* the next predicates are  renaming possible disjunctions on the rhs of initial    */
/* clauses. This is because in this program, the initial resolution is NOT          */
/* implemented. Instead, you have a simple check: if neg X (in the literal clauses) */
/* is generated, checks if X is in the initial set, generating false in the literal */
/* set of clauses.                                                                  */
/************************************************************************************/

check_initial(Initial,Final):-
   new_temp_prop(V),
   change_initial(Initial,V,NewList),
   append([start imp V],NewList,Final).

change_initial([],_,[]):-!.

change_initial([start imp X|Rest],V,[neg V or X|NewRest]):-
   change_initial(Rest,V,NewRest).

change_initial([H|Rest],V,[H|NewRest]):-
   change_initial(Rest,V,NewRest).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses.                                                                */
/********************************************************************************/

generate_snl((Initial,Literal,Modal,Temporal),(Initial,NewLiteral,NewModal,NewTemporal)):-
   my_write('\nGenerating SNL Clauses\n'),
   my_write('\nTemporal Clauses are \n'),
   write_ruleset(Temporal),
   generate_snl(Temporal,Literal,Modal,NewTemporal,NewLiteral,NewModal),
   my_write('\nNew Temporal Clauses are \n'),
   write_ruleset(NewTemporal).

generate_snl([],Literal,Modal,[],Literal,Modal).

generate_snl([r(N1,M,X imp next Y)|List],OldLiteral,OldModal,
 [r(N1,M,X imp next Y),r(N2,[N1],NewX imp next NewY)|NewList],NewerLiteral,NewerModal):-
   newrulecount(N2),
   snl(NewX,X,[N1],NewLiterals1,NewModals1),
   snl(NewY,Y,[N1],NewLiterals2,NewModals2),
   append(NewLiterals1,NewLiterals2,NewLiteral),
   append(NewLiteral,OldLiteral,Literal),
   append(NewModals1,NewModals2,NewModal),
   append(NewModal,OldModal,Modal),
   generate_snl(List,Literal,Modal,NewList,NewerLiteral,NewerModal).

generate_snl([X|List],OldLiteral,OldModal,[X|NewList],NewerLiteral,NewerModal):-
   generate_snl(List,OldLiteral,OldModal,NewList,NewerLiteral,NewerModal).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses, after resolution has been performed.                           */
/********************************************************************************/

generate_otter_snl(OldRules,NewOldRules,NewishLiteral,OldModal,SNewModal):-
   generate_snl(OldRules,[],OldModal,NewOldRules,NewLiteral,NewModal),
   remove_duplicate_rules(NewLiteral,NewerLiteral),
   remove_already_existing(NewerLiteral,NewOldRules,NewishLiteral),
   get_new_modal_rules(OldModal,NewModal,NewerModal),
   remove_duplicate_rules(NewerModal,SNewModal).

/* Remove already existing clauses */

remove_already_existing([],_,[]).

remove_already_existing([r(_,_,Rule)|Tail],OldRules,ReallyNew):-
   test_member(Rule,OldRules),!,
   remove_already_existing(Tail,OldRules,ReallyNew).

remove_already_existing([Head|Tail],OldRules,[Head|ReallyNew]):-   
   remove_already_existing(Tail,OldRules,ReallyNew).

/**********************************************************************************/

separate_and_format_rules(Rules,ERules,ERulePaths,(InitialRules,LiteralRules,ModalRules,TemporalRules)):-
   separate_rules(Rules,AllERules,InitialRules,LiteralRules,ModalRules,TemporalRules),
   remove_duplicate_erules(AllERules, ERules),
   my_write('\n\nThe list of eventuality rules is \n\n'),
   write_ruleset(ERules),
   process_eventualities(ERules, ERulePaths).

/*****************************************************************************/
/* separate_rules(Rules, ERules Others)                                      */
/*                                                                           */
/* This predicate takes a list of rules and splits it into two lists one     */
/* containing the separate_rules which have to be solved and the other       */
/* containg the remaining rules which are global box rules ie                */
/*           slast P imp F,                                                  */
/* where P is a conjunction and F a disjunction, any other rules being       */
/* disregarded.                                                              */
/*                                                                           */
/*****************************************************************************/

separate_rules([],[],[],[],[],[]):-!.

separate_rules([r(N1,N2, F imp sometime G)|Rules],
               [r(N1,N2, F imp sometime G)|ERules],InitialRules,LiteralRules,ModalRules,TemporalRules) :- !,
    separate_rules(Rules,ERules,InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add sometime clause to ERules


separate_rules([r(N1,N2,P imp next F)|Rules],
                ERules,InitialRules, LiteralRules, ModalRules,[r(N1,N2, P imp next F)|TemporalRules]) :- !,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add step clauses to TemporalRules

separate_rules([r(N1,N2,start imp F)|Rules],
               ERules,[r(N1,N2,F)|InitialRules],LiteralRules, ModalRules,TemporalRules) :- !,
   separate_rules(Rules,ERules,InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add initial clauses to InitialRules

separate_rules([r(N1,N2,X imp k A)|Rules],
               ERules,InitialRules,LiteralRules, [r(N1,N2, X imp k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,X imp neg k A)|Rules],
               ERules,InitialRules, LiteralRules, [r(N1,N2, X imp neg k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,F)|Rules],
               ERules,InitialRules,[r(N1,N2,F)|LiteralRules], ModalRules,TemporalRules) :-
   disjunction_of_literals(F),!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add literal clause to LiteralRules

separate_rules([_|Rules],ERules,InitialRules, LiteralRules, ModalRules,TemporalRules) :- 
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules,TemporalRules).
% Ignore other rules.

/*****************************************************************************/
/* remove_duplicate_erules(OldList, NewList).                                */
/*                                                                           */
/* Takes old list and removes duplicates to give NewList.                    */
/* N.B. Only works with  with initial and global eventuality rules and NOT   */
/* initial and global box rules.                                             */
/*                                                                           */
/* Two predicates have been maintained as the one above is called frequently */
/* during nontemporal resolution where as this one is only called            */
/* occasionally.                                                             */
/*****************************************************************************/

remove_duplicate_rules([],[]).

remove_duplicate_rules([r(_,_,Rule)|Rest], NewRules):-
   test_member(Rule, Rest),!,
   remove_duplicate_rules(Rest, NewRules).

remove_duplicate_rules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_rules(Rest, NewRules).

/*******************************************************/

remove_duplicate_erules([],[]).

remove_duplicate_erules([Rule|Rest], NewRules):-
   erule_in_list(Rule, Rest),!,
   remove_duplicate_erules(Rest, NewRules).

remove_duplicate_erules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_erules(Rest, NewRules).

/*****************************************************************************/
/* erule_in_list(Rule, List)                                                 */
/*                                                                           */
/* Takes a rule which is either an initial or global eventuality rule and    */
/* tests whether it is a member of List.                                     */
/*****************************************************************************/

erule_in_list(r(_,_, P imp sometime F), [r(_,_,Q imp sometime F)|_]):-
   strip(P, StrippedP),
   strip(Q, StrippedQ),
   same(StrippedP, StrippedQ).

erule_in_list(r(N1,N2, P imp sometime F), [_|Rest]):-
   erule_in_list(r(N1,N2, P imp sometime F), Rest).

/*********************************************************************************************/
/* process_eventualities(EList, ShortEList)                                                  */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventaulity with no duplicates and stores */
/* in a structured list as below.                                                            */
/*********************************************************************************************/

process_eventualities(EList, ShortEList):-
   extract_eventualities(EList, NewEList),
   remove_duplicate(NewEList, NewerEList),
   eventualities_paths(NewerEList,ShortEList).

/*********************************************************************************************/
/* extract_eventualites(EList, ShortEList)                                                   */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventuality with no duplicates            */
/*********************************************************************************************/

extract_eventualities([],[]).

extract_eventualities([r(_,_, _ imp sometime F)|Rest], [F|NewRest]):-
    extract_eventualities(Rest, NewRest).

/********************************************************************************************/
/*   eventualities_paths(EList,NewEList).                                                    */
/*                                                                                           */
/* Takes a list of eventuality rules (EList), and makes it into a list whose members are     */
/* sublists of the form [ERule, []]...etc where the [] is for filling with the previous paths*/
/* found for that eventuality.                                                               */
/*                                                                                           */
/*********************************************************************************************/

eventualities_paths([],[]).

eventualities_paths([H|Tail],[[H,[]]|NewTail]):-
  eventualities_paths(Tail,NewTail).

/*****************************************************************************************/

simplify_lists((Initial,Literal,Modal,Temporal),(SInitial,SLiteral,SModal,STemporal)):-
    simplify(Initial,SInitial),
    simplify(Literal,SLiteral),
    simplify(Modal,SModal),
    simplify(Temporal,STemporal).

/*****************************************************************************************/

step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules):-
   mres(Literal,Modal,NewerLiteral,[],Initial,NewLiteral,NewModal),
   do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal).

do_step_resolution(_,_,_,_,_,_,[r(0,[],false)],_):-
   otres_test_new_rules(_,r(0,[],false),_,_,_,_).

do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,_,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal):-
%   write(Literal),nl,write(Modal),nl,write(NewerLiteral),nl,write(NewLiteral),nl,
   my_write('step_resolution ==> New temporal rules are\n'),write_ruleset(NewerTemporal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral),
   my_write('step_resolution ==> New literal rules are\n'),write_ruleset(ExtraLiteral),
   write('\nstep_resolution ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nstep_resolution ==>Rules in the SoS are \n'),
   write_otter_rules(NewerTemporal,ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewModal,Temporal),ERulePaths,ERules,AllNewRules).

/**********************************************************************/

otres_test_otter_output([true imp next false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (1).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output([false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (2).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,([r(_,_,X)],Literal,_,Temporal),_,_,NewAllRules):-
   test_member(neg X,OldOtterRules),
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
   sort(NewishOldRules,SortedRules),
   my_write('\nOutput from otter ==> New literal rules are \n'),write_ruleset(NewOtterRules),
   my_write('\nOutput from otter ==> Remaining rules are\n'),write_ruleset(SortedRules),
   write('\nHave generated false (3).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
%   my_write('\nOldOtterRules - All Rules coming from otter\n'),write_ruleset(OldOtterRules),
%   my_write('\n\nNewOtterRules - Simplified Rules (p imp next false\n'),write_ruleset(NewOtterRules),
%   my_write('\n\nNewOldRules - Remaining rules\n'),write_ruleset(NewishOldRules),
    generate_otter_snl(NewishOldRules,SNLNewOldRules,NewLiteral,Modal,NewModal),
    append(NewOtterRules,NewLiteral,NewerOtterRules),
    my_write('\nOutput from otter ==> New literal rules (A imp next false) are \n'),write_ruleset(NewerOtterRules),
    my_write('\nOutput from otter ==> Remaining rules (after snl) are\n'),write_ruleset(SNLNewOldRules),
    my_write('\nOutput from snl   ==> New modal rules are\n'),write_ruleset(NewModal),
    run_tempres_or_otter(SNLNewOldRules,NewerOtterRules,NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

/**********************************************************************/

run_tempres_or_otter(NonTempRules,[],NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (1)\n'),
   split_temp_lit_rules(NonTempRules,NewLiteral,NewTemporal),
%   my_write('NonTempRules - All rules coming from otter\n'),write_ruleset(NonTempRules),
%   my_write('NewLiteral - Literal rules coming from otter\n'),write_ruleset(NewLiteral),
%   my_write('NewTemporal - Temporal rules coming from otter\n'),write_ruleset(NewTemporal),
   get_new_literal_rules_after_temporal(Literal,NewLiteral,SmallNewLiteral),
   my_write('Literal - Old Literal clauses\n'),write_ruleset(Literal),
   my_write('NewLiteral - All Literal clauses coming from otter\n'),write_ruleset(NewLiteral),
   my_write('SmallNewLiteral - the really new clauses generated by otter\n'),write_ruleset(SmallNewLiteral),
   run_tempres_or_modal(SmallNewLiteral,NewLiteral,NewModal,NonTempRules,NewTemporal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

run_tempres_or_otter(NonTempRules,[],_,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (2)\n'),
   write_ruleset(NonTempRules),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(NonTempRules,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(NonTempRules,MoreRules,(Initial,Literal,Modal,Temporal),NewERulePaths,ERules,AllNewRules).

run_tempres_or_otter(UsableRules,SoSRules,_,(Initial,Literal,Modal,Temporal),ERulePaths,ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (3)\n'),
   write('\nrun_tempres_or_otter ==> Usable rules are \n'),
   write_otter_rules(UsableRules),nl,
   write('\nrun_tempres_or_otter ==> Rules in the SoS are \n'),
   write_otter_rules(SoSRules),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_modal([],NewLiteral,_,AllTemporal,NewTemporal,(Initial,_Literal,Modal,_Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(AllTemporal,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,NewTemporal),NewERulePaths, ERules,AllNewRules).


run_tempres_or_modal(ExtraLiteral,NewLiteral,NewModal,_,NewTemporal,(Initial,Literal,Modal,_Temporal),
                                   ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (2)\n'),
   mres(Literal,Modal,ExtraLiteral,NewModal,Initial,FinalLiteral,FinalModal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral1),
%   my_write('\nLiteral'),
%   write_ruleset(Literal),
%   my_write('\nNewLiteral'),
%   write_ruleset(NewLiteral),
%   my_write('\nExtraLiteral1'),
%   write_ruleset(ExtraLiteral1),
   run_tempres_or_temporal(ExtraLiteral1,FinalLiteral,(Initial,Literal,FinalModal,NewTemporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_temporal(_,[r(_,_,false)],_,_,_,_):-
   otres_test_new_rules(_,r(_,_,false),_,_,_,_).

run_tempres_or_temporal([],NewLiteral,(Initial,_Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   append(Temporal,NewLiteral,AllTemporal),
   full_tempres(AllTemporal,ERulePaths, ERules,MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,Temporal),NewERulePaths, ERules,AllNewRules).

run_tempres_or_temporal(ExtraLiteral,NewLiteral,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (2)\n'),
%   k_to_literal(Modal,Kliteral),
   write('\nrun_tempres_or_temporal ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nrun_tempres_or_temporal ==> Rules in the SoS are \n'),
   write_otter_rules(ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewLiteral,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/*************************************************************************************/

otres_test_new_rules(_,r(N1,N2,false),_,_,_,r(N1,N2,false)):-
   write('Formula is unsatisfiable (1).\n').

otres_test_new_rules(_,[],_,_,_,[]):-
   write('Formula is satisfiable (2).\n').    % Note this really is satisfiable        
                     
otres_test_new_rules(OldRules,NewRules,(Initial,_,Modal,_),ERulePaths, ERules,NewAllRules):-
   split_temp_lit_rules(OldRules,Literal,Temporal),
   otres_allres((Initial,Literal,Modal,Temporal),NewRules,ERulePaths,ERules,NewAllRules).

/**********************************************************************/
/* rewrite_rules(OtterRules,OrdinaryRules,ImpFalseRules)              */
/*                                                                    */
/* Goes through the list of rules output by Otter and rewrites        */
/* rules of the form slast A imp false to their correct form and adds */
/* them to ImpFalseRules and after numbering copies the rest to       */
/* OrdinaryRules.                                                     */
/*                                                                    */
/**********************************************************************/

rewrite_rules([],[],[]).

rewrite_rules([false|_],[false],[]):-!.

rewrite_rules([P imp next false|Rules],NewOldRules,[NewRule1|NewRules]):- !,
  snff_negate(P, NegP),
  number_rule(NegP,[],NewRule1),
  rewrite_rules(Rules,NewOldRules,NewRules).

rewrite_rules([Rule|Rest],NewOldRules,NewRest):-
  number_rule(Rule,[],NRule),
  simplify_rule(NRule, SRule),
  test_add_rule(SRule,Rest,NewOldRules,NewRest).

/********************************************************************************************/

test_add_rule([],Rest,NewOldRules,NewRest):- !,
  rewrite_rules(Rest,NewOldRules,NewRest).
  
test_add_rule([Rule],Rest,[Rule|NewOldRules],NewRest):- 
  rewrite_rules(Rest,NewOldRules,NewRest).
  
/*********************************************************************************************/
/* full_tempres(Rules, ERulePaths, ERules, NewRules,NewERulePaths)                           */
/*                                                                                           */
/* Takes the current ruleset (Rules), a list of the eventualities with previous loop paths   */
/* found (ERulePaths), a list of the sometimes rules (ERules) and attempts temporal          */
/* between one of the eventualities and Rules. The path detected (LoopPath) is stored with   */
/* the appropriate eventuality, NewERule in ERulePaths, new resolvents from this loop path   */
/* and the eventuality are generated and stored in NewRules.                                 */
/*********************************************************************************************/

full_tempres(r(N1,N2,false), _, _, r(N1,N2,false), _,_).

full_tempres(_,[],ERules,NewRules,[],_):-
   write('\nfull_tempres (1) ==> No previous loop paths.\n'),
   process_path([],_,_,[],_,ERules,NewRules, _).

full_tempres(Rules,ERulePaths,ERules,NewRules,NewERulePaths,RulesUsed):-
   write('\nfull_tempres (2) ==> Previous loop paths.\n'),
   tempres(Rules,ERulePaths,NewERule,RelatedRules,RulesUsed,LoopPath),
   process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,NewERule,ERules,NewRules,NewERulePaths).

/*********************************************************************************************/
/*  process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules,          */
/*                                                           RotatedERulePaths).             */
/*                                                                                           */
/* Takes a loop found through a set of rules (LoopPath), a list of the rules which imply the */
/* current eventuality (RelatedRules), a list of the rule numbers which have been use in the */
/* loop we have found (RulesUsed), the list which stores each                                */
/* eventuality and the list of loops already found (ERulePaths), an eventuality (ERule) which*/
/* relates to this loop and a list of the sometimes rules (ERules). The new loop for this    */
/* ERule is addeded to ERulePaths. The ERulePaths list is rotated so the head is the         */
/* eventuality which followed ERule ie the new loop and ERule will now appear at the end of  */
/* the list. This is done in an attempt to find "easy" rules first. If the ERulePaths list   */
/* is cycled round then then one loop is found for each eventuality rule (if one exists) and */
/* so on rather than trying to find all the loops for each eventuality in turn. The new      */
/* resolvents are generated from this loop path to give NewRules and the updated and rotated */
/* ERulePathsare returned in RotatedERulePaths.                                              */
/*********************************************************************************************/

process_path([],_,_,_,_ERule,_, [], _):-
   my_write('No more loop paths.\n').

process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules, RotatedERulePaths):-
   write('The eventuality (for possibly several rules) being considered is '),
   write_form(ERule),nl,
   write('New Loop detected is '),
   write(LoopPath),nl,
   write('Loops previously detected are \n'), write('    [\n'),
   write_list(ERulePaths),
   write('    ]\n'),
   add_new_path_to_found_previously(ERule,LoopPath,ERulePaths,NodePath,NewERulePaths),
   rotate_rule(ERule,NewERulePaths,RotatedERulePaths),
   new_rules(ERule,LoopPath,ERules,RulesUsed,RelatedRules,NodePath,NewRules),
   write_ruleset(NewRules).

/*********************************************************************************************/
/*  rotate_rule(ERule,ERulePaths,NewList)                                                    */
/*                                                                                           */
/* Take an eventuality ERule, and a list of eventualities paired with previous loop paths    */
/* found (ERulePaths). It searches for ERule in ERulePaths and makes the eventuality         */
/* following ERule in the list the head and the portion of the list from the front to where  */
/* ERule was located to the end of the new list (NewList). This was an attempt to find all   */
/* "easy" loops first. Rotating the eventuality rules in this manner means that the temporal */
/* resolution process will try to find a loop for each eventuality (if one exists) and then  */
/* a second, and third etc.                                                                  */
/*********************************************************************************************/

rotate_rule(ERule,ERulePaths,NewList):-
     split_list(ERule,ERulePaths,FirstBit, LastBit),
     append(LastBit,FirstBit,NewList).

/*********************************************************************************************/
/*   split_list(ERule,ERulePaths,FirstBit, LastBit)                                          */
/*                                                                                           */
/* Takes an eventuality ERule, and a list of eventualities paired with loop paths previously */
/* found (ERulePaths). It searches down ERulePaths looking for an ERule in the list. It      */
/* stores all the things before this and including this as FirstBit, and all the remaining   */
/* stuff as LastBit. The aim is the rotate the eventualities and lists in ERulePaths to try  */
/* attempt to find a loop for each eventuality in turn.                                      */
/*********************************************************************************************/

split_list(ERule,[[ERule,PrevPath]|Rest],[[ERule,PrevPath]], Rest).

split_list(ERule,[[AnERule,PrevPath]|Rest],[[AnERule,PrevPath]|FirstBit], LastBit):-
   split_list(ERule,Rest,FirstBit,LastBit).

/*********************************************************************************************/
/*  new_rules(ERule, CycleRules, ERules, RulesUsed,RelatedRules,NodePath, NewRules)          */
/*                                                                                           */
/*  Takes the eventuality (ERule) we have detected a loop for, and a list of rules           */
/*  (CycleRules) which have been generated from the the loop detected, and the complete      */
/*  list of eventuality rules (ERules) and returns a list of new resolvents (NewRules).      */
/*                                                                                           */
/*********************************************************************************************/

new_rules(ERule, CycleRules, ERules, _RuleNoUsed,RelatedRules,NodePath,NewRules):-
  get_rel_rules(ERule, ERules, RelRules),
  generate_new_rules(RelRules, CycleRules, _RuleNoUsed,RelatedRules,NodePath,NewRules).

/*********************************************************************************************/
/* add_new_path_to_found_previously(ERule,NewPath,ERulePaths,NodePath,NewERulePaths)         */
/*                                                                                           */
/* The new loop path NewPath found for the current eventuality ERule is added                */
/* to the list ERulePaths which contains a list of eventualities and the                     */
/* previously discovered.                                                                    */
/*                                                                                           */
/*********************************************************************************************/

add_new_path_to_found_previously(ERule,NewPath,[[ERule, OldPaths]|Rest],_NodePath,[[ERule,[NewPath|OldPaths]]|Rest]).

add_new_path_to_found_previously(ERule,NewPath,[ERulePath|Rest],NodePath,[ERulePath|NewRest]):-
   add_new_path_to_found_previously(ERule,NewPath,Rest,NodePath,NewRest).

/*********************************************************************************************/
/* get_rel_rules(Eventuality, ERules, RelRules)                                              */
/*                                                                                           */
/* Takes the eventuality (Eventuality) for which a loop has been found, and the list of      */
/* rules with eventualities (ERules) and picks out the rules related to the current          */
/* eventuality.                                                                              */
/*********************************************************************************************/

get_rel_rules(_, [], []).

get_rel_rules(F, [r(N1,N2,P imp sometime F)|Rest], [r(N1,N2,P imp sometime F)|NewRest]):-
    get_rel_rules(F, Rest, NewRest).

get_rel_rules(F, [_|Rest], NewRest):-
    get_rel_rules(F, Rest, NewRest).

/*********************************************************************************************/
/* generate_new_rules(RelRules, CycleRules, RuleNoUsed,RelatedRules,NodePath,NewRules)       */
/*                                                                                           */
/* Takes a list of eventuality rules (RelRules) which are related to the eventuality in      */
/* question, and a list of rules (CycleRules) which have generated from the loop detected    */
/* from the eventuality in question, a list of the numbers of the rules used in the loop     */
/* detected (RuleNoUsed), a list of rules of the form slast R imp p, where "p" is the        */
/* eventuality being currently considered (RelatedRules), a list of the nodes used in the    */
/* loop ( and unstructured list) (NodePath) and produces a list of new resolvents.           */
/*********************************************************************************************/

generate_new_rules([],_,_,_,_,[]).

generate_new_rules([RelRule|Rest],CycleRules, _RuleNoUsed,RelatedRules,NodePath,AllNewRules):-
   generate_new_rule(RelRule, CycleRules, _AllRulesUsed,SomeNewRules),
   generate_new_rules(Rest, CycleRules, _AllRulesUsed,RelatedRules, NodePath,OtherNewRules),
   append(SomeNewRules, OtherNewRules, AllNewRules).

/*********************************************************************************************/
/*   generate_new_rule(RelRule, CycleRules, RuleNoUsed, SomeNewRules)                        */
/*                                                                                           */
/* Takes an eventuality rule (RelRule), a set of rules we have found a loop in               */
/* (CycleRules), and a list of the rule numbers used in the loop detected (RuleNoUsed) and   */
/* generates the new resolvents, (SomeNewRules).                                             */
/*********************************************************************************************/

generate_new_rule(r(N1,_, A imp sometime F), [[true]],_RuleNoUsed,[r(0,[N1],NegA or F)]):-
      snff_negate(A,NegA).

generate_new_rule(r(N1,_,true imp sometime F), [[NegF]],_RuleNoUsed,[r(0,[N1],F)]):-
      disjunction_of_literals(F),
      snff_negate(F, NegF).

generate_new_rule(r(_N1,_,P imp sometime F),DisjunctList,_RuleNoUsed,NewRules):-
      neg_cycle_list(DisjunctList,NewList),
      construct_new_rules(P,NewList,F,NewRules).

/***************************************************************************************/

construct_new_rules(P, List, Eventuality,[V imp next (V or Eventuality), NegP or V or Eventuality |Rest]):-
      snff_negate(P, NegP),
      new_temp_prop(V),
      build_rules(NegP,Eventuality,V,List,Rest). 

/********************************************************************************************/

build_rules(NegP,Eventuality,V,[H|Tail],[NegP or Eventuality or H, V imp next (H or Eventuality)|Rest]):-
      build_rules(NegP,Eventuality,V,Tail,Rest).

build_rules(_NegP,_Eventuality,_V,[],[]).

/*********************************************************************************************/
/*       neg_cycle_list(List,NewList).                                                       */
/*                                                                                           */
/* Takes a list (List) which is made up of sublists where the sublists are conjuncts and the */
/* main list is disjunctive. This predicate negates each item in the sublists and disjoins   */
/* them. The sublists are then conjoined together and returned in a NewList.                 */
/*********************************************************************************************/

neg_cycle_list([[P]],[NegP]):-
     snff_negate(P,NegP).

neg_cycle_list([List],[NegP]):-
     snff_negate_list(List,NegP).

neg_cycle_list([List|Lists], [NegP| Rest]):-
     snff_negate_list(List,NegP),
     neg_cycle_list(Lists,Rest).

/**************************************************************************************/

snff_negate_list([H],NegH):-
   snff_negate(H,NegH).

snff_negate_list([H|Rest],NegH or NewRest):-
   snff_negate(H,NegH),
   snff_negate_list(Rest,NewRest).

/**************************************************************************************/

split_temp_lit_rules([r(N,M,A imp next B)|Rest],Literal,[r(N,M,A imp next B)|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([r(N,M,B)|Rest],[r(N,M,B)|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([A imp next B|Rest],Literal,[A imp next B|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([B|Rest],[B|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([],[],[]).

/*******************************************************************************/

get_new_modal_rules(Modal,[r(N,_,_)|NewModal],Other):-
   rule_member(N,Modal),!,
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(Modal,[r(_,_,Rule)|NewModal],Other):-
   test_member(Rule,Modal),
   get_new_modal_rules(Modal,NewModal,Other).
   
get_new_modal_rules(Modal,[r(N,M,Rule)|NewModal],[r(N,M,Rule)|Other]):-
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(_,[],[]).

/******************************************************************************/

get_new_literal_rules(Literal,[r(N,_,_)|NewLiteral],Other):-
   rule_member(N,Literal),!,
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(_,[],[]).

/********************************************************************************/

get_new_literal_rules_after_temporal(Literal,[r(_,_,Rule)|NewLiteral],Other):-
   rule_in_list(Rule,Literal),!,
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(_,[],[]).

/**************************************************/

rule_in_list(Rule,[r(_,_,Rule)|_]):- !.

rule_in_list(D,[r(_,_,E)|_]):-
    strip(D,SD),
    strip(E,SE),
    same(SD,SE),!.

rule_in_list(D,[_|Rest]):-!,
    rule_in_list(D,Rest).

/*******************************************/

rule_member(N,[r(N,_,_)|_]):- !.

rule_member(N,[_|Rest]):- 
   rule_member(N,Rest).

rule_member(_,[]):-!,fail.

/*****************************************/

test_member(X,[X|_]).

test_member(X,[r(_,_,X)|_]).

test_member(X,[_|Rest]):-
    test_member(X,Rest).

/**********************************************************/
/* Gives the clauses coming from Otter their old numbers. */
/* Previously, clauses were just renumbered, making the   */
/* reading of the proof difficult.                        */
/**********************************************************/

give_rules_old_numbers([],_,_,[]).

give_rules_old_numbers([r(N1,N2,X imp next Y)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X imp next Y),Temporal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

give_rules_old_numbers([r(N1,N2,X)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X),Literal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

find_same_rule(r(N1,N2,Clause),List,r(N1,N2,Clause)):-
   not(member(r(_,_,Clause),List)).

find_same_rule(r(N1,N2,Clause),List,r(N5,N6,Clause)):-
   sublist(same_rule(r(N1,N2,Clause)),List,SameList),
   sort(SameList,Sorted),
   nth1(1,Sorted,r(N3,N4,Clause)),
   test_number(N1,N2,N3,N4,N5,N6).

same_rule(r(_,_,X),r(_,_,Y)):-
   strip_or(X,SX),
   strip_or(Y,SY),
   flatten(SX,FX),
   flatten(SY,FY),
   subset(FX,FY),
   subset(FY,FX).

test_number(N1,N2,N3,_N4,N1,N2):- N1 < N3.
test_number(_N1,_N2,N3,N4,N3,N4).
