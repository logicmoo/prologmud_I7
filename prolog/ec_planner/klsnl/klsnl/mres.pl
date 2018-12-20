/**************************************************************************/
/*
   This program is an implementation of nontemporal resolution. 
The predicates related to substitution have been written by Michael, 
non temporal resolution predicates are based on those written by Michael, 
and subsumtion predicates are written by Clare.

The following procedures are necessary :-

        1. Check for false
        2. Simplification ie x => next false converted to
                             true => ~x  
        3. Subsumption
        4. Non temporal resolution
        5. Provides new real resolvents
        6. Repeat 1 to 5 until false found or no new resolvents

Note :-
======
The predicate
                fullmres(OldRules,NewRules)

sets the non temporal resolution in progress and expects a set of clauses derived 
from translation to SNF (OldRules),which have been numbered and rewritten to the form

                r(Number,FromList,Text)

where Number is the number of the clause, FromList, will originally be empty, and Text 
is the actual clause itself. It returns NewRules ie what has been derived from non 
temporal resolution. This predicate is called from fullres.pl as part of the main cycle.

     A predicate cyclemres is called which recurs on itself until no new resolvents have 
been derived or false has been derived. This predicate and the predicates it calls maintains 
a distinction round each cycle of the Old ruleset and those newly derived from resolution, 
so simpilfication may just be done on the new ruleset, subsumption may be done between the 
new ruleset and the old ruleset, and then non temporal resolution may be done between the 
new ruleset and the old ruleset,before combining the two and starting again around the cycle.

********************************************************************************/

/*******************************************************************************/
/*  mres(LiteralRules,
         ModalRules,
         InitialRules,
         NewLRules,
         NewMRules)                                                            */


/* This sets the nontemporal resolution off, the first three arguments,        */
/* containing literal, modal and initial clauses,                              */
/* derived from SNF which have been rewriten into the form                     */
/* r(Number,FromList,Text), where currently FromList will be empty.            */
/* NewLRules and NewMRules will contain the new ruleset after no more          */
/* non temporal resolution can take place, or false if false has been derived. */
/*******************************************************************************/

mres(LiteralRules,ModalRules,Initial,NewLRules,NewMRules):-
     cyclemres(1,[],[],LiteralRules,ModalRules,Initial,NewLRules,NewMRules).    

% Leave 2nd and 3rd arguments empty so as to treat the original ruleset as "new" resolvents.

/*****************************************************************************/
/*                                                                           */
/* This predicate is called from fullres.pl during the full resolution       */
/* process. OldRules are in the form r(Number,FromList,Text), where          */
/* Number is the ruless number, FromList contains the numbers of the rules   */
/* which were the parents of this resolvent, and Text is the rule itself     */
/* (in SNF). NewishRules contain the new resolvents obtained from the        */
/* last temporal resolution step. SRules contains a rules which have been    */
/* subsumed on previous cycles round the non-temporal resolution process.    */
/* NewRules will contain the new ruleset                                     */
/* after no more non temporal resolution can take place, or false if a       */
/* contradiction has been derived. NewSRules contains SRules plus any new    */
/* rules which ahve been subsumed.                                           */
/*****************************************************************************/

mres(OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal):-
   cyclemres(1,OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal).

/*****************************************************************************/
/*  cyclemres(Count,
              OldLiteral,
              OldModal,
              NewishLiteral,
              NewishModal,
              Initial,
              NewLiteral,
              NewModal
              )                                                              */
/*                                                                           */
/*  Cycles around the non temporal resolution cycle, checking for false and  */
/*  if found returning false, otherwise performing simplification,           */
/*  subsumption, nontemporal resolution and then recurring on itself until   */
/*  false or an empty set of resolvents is detected. Count is the current    */
/*  cycle we are on, OldRules are rules from previous cyles rounds the       */
/*  non-temporal resolution processes. NewishResolvents are the new          */
/*  resolvents generated during the last cycle of non-temporal resolution    */
/*  and SRules are the store of rules which have been subsumed which may need*/
/*  to be kept to generate a proof if false is derived. NewRules is the      */
/*  final ruleset after no more non-temporal resolution can be done and      */
/*  NewSRules is the new set of subsumed rules.                              */
/*****************************************************************************/

cyclemres(_,[],[],[],[],_,[],[]).
%:-my_write('\ncyclemres ==> (1)\n').                     % No new resolvents.

cyclemres(_,LRules,MRules,[],[],_,LRules,MRules):-
%    my_write('\ncyclemres ==> (2)\n'),
    my_write('Have generated no new resolvents,current literal/modal clauses are :-\n'),
    write_ruleset(LRules),                   % Terminate non temporal resolution
    write_ruleset(MRules).                   % as no new Rules have been derived.

cyclemres(_,_,_,NewLRules,_,[r(_,_,X)],[r(0,[],false)],_):-
%    my_write('\ncyclemres ==> (2.5)\n'),
    snff_negate(X,NegX),
    test_member(NegX,NewLRules),
    simplify(NewLRules,SNewLRules),
    self_subsumption(SNewLRules,SubNewLRules),
    flatten(SubNewLRules,FSubNewLRules),
    my_write('\nNew literal clauses include (neg start).\n'),
    write_ruleset(FSubNewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in initial clauses.\n').

cyclemres(_,_OldLRules,OldMRules,NewLRules,_,_,[r(0,[],false)],OldMRules):-
%    my_write('\ncyclemres ==> (3)\n'),
    test_member(false,NewLRules),
    my_write('\nNew literal clauses include false.\n'),
    write_ruleset(NewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in literal clauses.\n').

cyclemres(_,_OldLRules,_OldMRules,_,NewMRules,_,[r(0,[],false)],[r(0,[],false)]):-
%    my_write('\ncyclemres ==> (4)\n'),
    test_member(false,NewMRules),
    my_write('\nNewModal clauses include false.\n'),
    write_ruleset(NewMRules),   % Terminate non temporal resolution as false
    write('\nHave generated false in modal clauses.\n').

% Occasionally ran out of space during non-temporal resolution so have hadded a specific garbage collect.

cyclemres(Count,OldLRules,OldMRules,NewLRules,NewMRules,Initial,NewerLRules,NewerMRules):-
%   my_write('\ncyclemres ==> (5)\n'),
%   garbage_collect,
   writef('\nCycle Number%t.',[Count]),
   my_k_to_literal(NewMRules,KRules),
   append(KRules,NewLRules,AddedNewLRules),
   simplify(AddedNewLRules,SimplifiedLRules),
   simplify(NewMRules,SimplifiedMRules),
   garbage_collect,
   subsumption(OldLRules,SimplifiedLRules,SubOldLRules,SubNewLRules1),
   trim_stacks,
   subsumption(OldMRules,SubNewLRules1,SubOldMRules1,SubNewLRules2),
   trim_stacks,
   subsumption(SimplifiedMRules,SubNewLRules2,SubNewMRules1,SubNewLRules),
   trim_stacks,
   subsumption(SubOldMRules1,SubNewMRules1,SubOldMRules,SubNewMRules),
   trim_stacks,
   my_write('\nOld Literal Clauses\n'),
   write_ruleset(SubOldLRules),
   my_write('\nOld Modal Clauses\n'),
   write_ruleset(SubOldMRules),
   my_write('\nNew Literal Clauses\n'),
   write_ruleset(SubNewLRules),
   my_write('\nNew Modal Clauses\n'),
   write_ruleset(SubNewMRules),
   do_mres(SubOldLRules,SubOldMRules,SubNewLRules,SubNewMRules,ResLRules,ResMRules,NewOldLRules,NewOldMRules),
   NewCount is Count + 1,!,garbage_collect,
   cyclemres(NewCount,NewOldLRules,NewOldMRules,ResLRules,ResMRules,Initial,NewerLRules,NewerMRules).

/* ************************ */
/* SUBSUMPTION              */
/* ************************ */

/**********************************************************************************/
/*  subsumption(Old_Rules,New_Rules,SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                */
/*   This predicate takes every rule in New_Rules and attempts subsumption        */
/*   between every rule and every other in this list. Then subsumption is         */
/*   performed between everything in the subsumed version of New_Rules and        */
/*   Old_Rules. New_Old_Rules is then the subsumed version of Old_Rules and       */
/*   New_New_Rules is the subsumed version of New_Rules. SRules contains rules    */
/*   which have been subsumed on previous cycles and NewSRules contain            */
/*   SRules plus any rules subsumed on this cycle.                                */
/**********************************************************************************/

subsumption(Old_Rules,New_Rules,New_Old_Rules,FNew_New_Rules) :-
  self_subsumption(New_Rules,Newish_Rules),
  flatten(Newish_Rules,FNewish_Rules),!,
  subsumption2(Old_Rules,FNewish_Rules,New_Old_Rules,New_New_Rules),
  flatten(New_New_Rules,FNew_New_Rules),!.

/******************************************************************************/
/*  self_subsumption([R | Rest],SRules,New_Rules,NewSRules)                   */
/*                                                                            */
/*  Attempts subsumption between R and Rest, recurrs on Rest and outputs      */
/*  subsumed list in New_Rules. SRules are the rules which have been subsumed */
/*  on previous cycles and any new ones subsumed are added to NewSRules.      */
/******************************************************************************/

self_subsumption([],[]):-!.

self_subsumption([R|Rest],[NewR|NewRules]):-
  subsume_rule(Rest,R,Remain_Rest,NewR),!,
  self_subsumption(Remain_Rest,NewRules).


/***********************************************************************************/
/*  subsumption2(OldRules,[R | Rest],SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                 */
/*  Attempts subsumption between R and OldRules, recurrs on Rest and outputs       */
/*  lists after subsumption in New_Old_Rules and New_New_Rules. SRules are the     */
/*  rules which have been subsumed on previous cycles and any new ones             */
/*  subsumed are added to NewSRules.                                               */
/***********************************************************************************/

subsumption2(Old_Rules,[],Old_Rules,[]):-!.

subsumption2([],NewRules,[],NewRules):-!.

subsumption2(Old_Rules,[R|Rest],New_Old_Rules,[NewR|New_Rules]):-
   subsume_rule(Old_Rules,R,Newish_Old_Rules,NewR),!,
   subsumption2(Newish_Old_Rules,Rest,New_Old_Rules,New_Rules).

/*******************************************************************************/
/* subsume_rule(Old_Rules,ThisRule,SRules,New_Old_Rules,New_ThisRule,NewSRules)*/
/*                                                                             */
/*   This predicate compares ThisRule with every rule in Old_Rules. Rules are  */
/*   in the format r(Number,FromList,Text). If ThisRule is subsumed by a       */
/*   rule in Old_Rules then Old_rules are returned and New_ThisRule will be    */
/*   and empty list but if ThisRule subsumes a Rule in Old_Rules, the rule     */
/*   that has been subsumed is removed from Old_Rules and the predicate the    */
/*   attempts subsumption between ThisRule and the rest of Old_Rules.          */
/*   It is important that the predicates are in the order they appear in, as   */
/*   if there are duplicate rules ie Rule1 subsumes Rule2 and vice versa we    */
/*   want to remove ThisRule and return an empty list as New_ThisRule          */
/*   rather than the other way around for we are depending upon no new         */
/*   resolvents to be derived for our non-temporal resolution process to       */
/*   terminate. SRules contains any rules which have been subsumed previously  */
/*   NewSRules will also contain rules subsumed on this cycle.                 */
/*******************************************************************************/

% Firstly looking at cases of strong last implies something

subsume_rule([],r(N1,N2,G),[],[r(N1,N2,G)]):-!.
subsume_rule([],R,[],[R]):-!.

subsume_rule([r(N1,N2,F) | Rules],r(_,_,G),[r(N1,N2,F) | Rules],[]) :-
             dis_implies(F,G),!.                                % P => F

subsume_rule([r(_,_,F) | Rules],r(N1,N2,G),New_Old_Rules,New_New_Rules) :-
            dis_implies(G ,F),!,                        % Q => G subsumes P => F
	    subsume_rule(Rules,r(N1,N2,G),New_Old_Rules,New_New_Rules).

% default behaviour: ie no subsumption takes place

subsume_rule([ R | Rules],r(N1,N2,G),[ R | New_Rules],New_New_Rules) :-
	subsume_rule(Rules,r(N1,N2,G),New_Rules,New_New_Rules).

% default behaviour to catch any of the cases not mentioned above

subsume_rule([ R1 | Rules],R2,[ R1 | New_Old_Rules],New_New_Rules) :-
   subsume_rule(Rules,R2,New_Old_Rules,New_New_Rules).


/* ************************* */
/* NON-TEMPORAL RESOLUTION * */
/* ************************* */

/*********************************************************************************/
/* mres(OrigRules,[Rule|Rest],NewRules,Final)                                    */
/*                                                                               */
/*   Michaels code changed slightly to do non temporal resolution between        */
/*   two lists OrigRules and [Rule|Rest] (new rules derived from resolution last */
/*   time round the loop). After resolving Rule with every rule in OrigRules it  */
/*   is added to OrigRules, and the first rule from Rest is resolved with all    */
/*   rules in OrigRules + Rule, and so on. When Rest is empty, OrigRules which   */
/*   now has all of [Rule|Rest] attached is copied to NewOrig and resolvents     */
/*   found built up in Final. The first time round the loop ie after             */
/*   initial conversion to SNF OrigRules will be empty, so gradually [Rule|Rest] */
/*   will be copied into it and effectively resolved on itself. Rules are in the */
/*   form r(Number,FromList,Text) . When resolution takes place a new            */
/*   number for the rule is placed in Number and FromList will have the two rule */
/*   numbers of the parents of the new rule.                                     */
/*********************************************************************************/


do_mres(LNewOrig,MOrigRules,LNew,RNew,FinalL,FinalM,NewOrigL2,NewOrigM2):-
%    my_write('\ndo_mres ==> (2)\n'),
    do_mres(LNewOrig,MOrigRules,LNew,FinalL1,FinalM1,NewOrigL1,NewOrigM1),
    do_mres(NewOrigL1,NewOrigM1,RNew,FinalL2,FinalM2,NewOrigL2,NewOrigM2),
    append(FinalL1,FinalL2,FinalL),
    append(FinalM1,FinalM2,FinalM).

do_mres(LNewOrig,MNewOrig,[],[],[],LNewOrig,MNewOrig).
%:-my_write('\ndo_mres ==> (1).\n').

do_mres(LOrigRules,MOrigRules,[Rule|Rest],FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\ndo_mres ==> (3).\n'),
	mresolve(Rule,LOrigRules,MOrigRules,RulesL1,RulesM1),
        add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM).

/*************************************************************************************************/

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (1)\n'),
        is_modal_rule(Rule),!,
	do_mres(LOrigRules,[Rule|MOrigRules],Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (2)\n'),
	do_mres([Rule|LOrigRules],MOrigRules,Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

/*********************************************************************************/
/*  mresolve(Rule,OrigRules,NewRules)                                            */
/*                                                                               */
/*  Takes a rule of the form r(Number,FromList,Text) and tries resolution        */
/*  between the Text of this rule and the Text of all the other rules in         */
/*  OrigRules. Any new resolvents derived are put into new rules where the       */
/*  Number of the new resolvent is the next new rule number, and FromList is     */
/*  filled with the rule numbers of the parents of the resolvent.                */
/*********************************************************************************/

/* First,the cases where resolution cannot take place */

mresolve(_,[],[],[],[]).
% :-my_write('\nmresolve ==> (1)\n').

/* Next,resolution of initial rules: */

mresolve(r(N1,N2,F),[r(N3,_,G) | Rest],Other,LFinal,MFinal) :-
%        my_write('\nmresolve ==> (2)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),Rest,Other,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[_ | Rest],Other,NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (3)\n'),
	mresolve(Rule,Rest,Other,NewLRules,NewMRules).

mresolve(r(N1,N2,F),[],[r(N3,_,G) | Rest],LFinal,MFinal) :-
%        my_write('\nmresolve ==> (4)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),[],Rest,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[],[_ | Rest],NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (5)\n'),
	mresolve(Rule,[],Rest,NewLRules,NewMRules).

/*****************************************************************************/
/*  generate_new_resolvent(FromList,ResolventList,Rules,NewRules             */
/*                                                                           */
/* If Resolvent is empty this just returns Rules as they are, otherwise a    */
/* new rule is created and added to the front of Rules.                      */
/*****************************************************************************/

generate_new_resolvent(_,[],LR,MR,LR,MR).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,FinalLRules,FinalMRules):-
      is_modal_disjunct(Resolvent),!,
      simplify_rule(r(_,FromList,Resolvent),[r(_,FromList,SimpRule)]),
      change_form(SimpRule,RulesList),
      number_rules(RulesList,FromList,NumberedRules),
      simplify(NumberedRules,SimpRules),
      separate_rules(SimpRules,_ERules,_Initial,NewLRules,NewMRules,_Temporal),
      append(LRules,NewLRules,FinalLRules),
      append(MRules,NewMRules,FinalMRules).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,[SimpRule | LRules],MRules):-
      newrulecount(N),
      simplify_rule(r(N,FromList,Resolvent),[SimpRule]).

/******************************************************************************/
/*  basic_nt_resolve(Disj1,Disj2,Resolvent,S1,S2) :-                          */
/*                                                                            */
/*      Tries to resolve the clauses Disj1 and Disj2 together.                */
/*      If it fails, Resolvent is [], if it succeeds,                         */
/*	resolvent is [resolvent]. S1 is a list recording what is left          */
/*	of Disj1, while S2 is what is left of Disj2.                           */
/******************************************************************************/ 

basic_nt_resolve(A imp B,Disj2,Resolvent,S1,S2):- 
        snff_negate(A,NegA),!,
        basic_nt_resolve(NegA or B,Disj2,Resolvent,S1,S2).        

basic_nt_resolve(Disj1,A imp B,Resolvent,S1,S2):-
        snff_negate(A,NegA),!,
        basic_nt_resolve(Disj1,NegA or B,Resolvent,S1,S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(A,Disj2,Resolvent,[B|S1],S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(B,Disj2,Resolvent,[A|S1],S2).

basic_nt_resolve(_ or _, _,_,_,_) :- fail.

basic_nt_resolve(A,Disj2,Resolvent,S1,S2) :-
	internal_nt_resolve(A,Disj2,Resolvent,S1,S2).

/*****************************************************************************/
/*  internal_nt_resolve(L,Disj, Resolvent,S1,S2)                             */
/*                                                                           */
/*  Attempts to resolve L which is now a proposition or negated proposition  */
/*  with Disj. S1 and S2 have what remains of the original two disjuncts     */
/*  we were trying to resolve. The resolvent created is returned in          */
/*  Resolvent                                                                */
/*****************************************************************************/

internal_nt_resolve(L,A or B, Resolvent,S1,S2) :-
	internal_nt_resolve(L,A,Resolvent,S1,[B|S2]).

internal_nt_resolve(L,A or B,Resolvent,S1,S2) :- 
	internal_nt_resolve(L,B,Resolvent,S1,[A|S2]).

internal_nt_resolve(_,_ or _,_,_,_) :- fail.

internal_nt_resolve(L,M,[Resolvent],S1,S2) :-
        resolveable(L,M,S1,S2,NewS1,NewS2),
	append(NewS1,NewS2,NewLst),
	disjoin(NewLst,Resolvent).

/*********************/
/* Basic Resolution  */
/*********************/

resolveable(L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

/********************/
/* Modal Resolution */
/********************/

resolveable(L,M,S1,S2,NewS1,NewS2):-
  sometime_rule(L,M,S1,S2,NewS1,NewS2). 

resolveable(L,M,S1,S2,NewS1,NewS2):- 
  always_rule(L,M,S1,S2,NewS1,NewS2). 

/**********/ 
/* MRES 2 */ 
/**********/ 

sometime_rule(k L,k M,S1,S2,S1,S2):-
  comp_lits(L,M). 

/**********/ 
/* MRES 3 */ 
/**********/ 

sometime_rule(k L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

sometime_rule(L,k M,S1,S2,S1,S2):-
  comp_lits(L,M).

/**********/ 
/* MRES 4 */ 
/**********/ 

always_rule(neg k L,L,S1,S2,S1,NewS2):-
  change_resolvent(S2,NewS2).

always_rule(L,neg k L,S1,S2,NewS1,S2):-
  change_resolvent(S1,NewS1).

always_rule(neg k neg L,M,S1,S2,S1,NewS2):-
  comp_lits(L,M), 
  change_resolvent(S2,NewS2). 

always_rule(L,neg k neg M,S1,S2,NewS1,S2):- 
  comp_lits(L,M), 
  change_resolvent(S1,NewS1). 

/****************/ 
/* MOD FUNCTION */ 
/****************/ 

change_resolvent([A or B|Rest], NewAll):-
  change_resolvent(A or B,NewAB),
  change_resolvent(Rest, NewRest), 
  append(NewAB,NewRest, NewAll).

change_resolvent(A or B, NewAB):-
  change_resolvent(A, NewA),
  change_resolvent(B, NewB),
  append(NewA, NewB, NewAB).

change_resolvent([A|Rest],NewAll):-
  change_resolvent(A,NewA),
  change_resolvent(Rest,NewRest),
  append(NewA,NewRest,NewAll).

change_resolvent([],[]).

change_resolvent(X,[X]):-is_snl_literal(X).

change_resolvent(neg k L,[neg k L]):-is_literal(L).

change_resolvent(k L,[k L]):-is_literal(L).

change_resolvent(k neg L,[k neg L]):-is_literal(L).        % Clare is this right?

change_resolvent(neg k neg L,[neg k neg L]):-is_literal(L).

change_resolvent(neg L,[neg k L]):-is_literal(L).

change_resolvent(L,[neg k neg L]):-is_literal(L). 

/**********************************/ 

is_modal_rule(r(_,_,Y imp X)):- 
   is_modal_rule(Y imp X). 

is_modal_rule(_Y imp X):- 
  is_modal_disjunct(X). 

/**********************************/ 

is_modal_disjunct(k _). 

is_modal_disjunct(neg k _). 

is_modal_disjunct(A or _):- 
  is_modal_disjunct(A). 

is_modal_disjunct(_ or B):- 
  is_modal_disjunct(B). 

/***************************************/

is_list_of_only_modals(A or B):-
  is_modal(A), 
  is_list_of_only_modals(B).

is_list_of_only_modals(A):-
  is_modal(A).

is_list_of_only_literals(A or B):-
  is_literal(A), 
  is_list_of_only_literals(B).

is_list_of_only_literals(A):-
   is_literal(A). 

/*****************************************************************/
/* The following returns a modal clause in the new normal form   */
/*****************************************************************/ 

change_form(true,[true]).

change_form(Disjunction,[true imp Disjunction]):-
  is_modal(Disjunction).

change_form(neg k X or Disj1, [Disj2 imp neg k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(k X or Disj1, [Disj2 imp k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or neg k X, [Disj2 imp neg k X] ):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or k X, [Disj2 imp k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disjunction,[Disjunction]):-
   is_list_of_only_literals(Disjunction).

change_form(Disjunction,List):- 
  is_list_of_only_modals(Disjunction), 
  snl_form(Disjunction,Literals,Modals),
  append([Literals],Modals,List). 

change_form(Disj1 or Disj2,Rest):-
  is_literal(Disj1), 
  snff_rewrite_list([neg Disj1 imp (Disj2)],Rest).

change_form(Disj1 or Disj2,Rest):- 
  is_not_literal(Disj1),!, 
  change_order(Disj1 or Disj2, NewDisj),
  change_form(NewDisj,Rest). 

/************************************************/ 

change_order(A or B,NewOrder):- 
  strip_or(A,NewA), 
  strip_or(B,NewB), 
  flatten(NewA,FlatA), 
  flatten(NewB,FlatB), 
  append(FlatB,FlatA,New), 
  disjoin(New,NewOrder). 


/*********************************************************************/
/* The following returns the SNL literals and definition clauses     */
/* from a given formula                                              */
/*                                                                   */
/* (1) the SNL literal itself                                        */
/* (2) the disjunction of SNL literals                               */
/* (3) the SNL(NEW), in case of a conjunction                        */
/* (4) SNL(literal), in case of a non SNL literal                    */
/*                                                                   */
/*********************************************************************/

snl(Formula,Formula,_,[],[]):- 
   is_snl_literal(Formula). 

snl(NewLiteralA or NewLiteralB,A or B,N,Literals,Modals):- 
   snl(NewLiteralA,A,N,Literals1,Modals1), 
   snl(NewLiteralB,B,N,Literals2,Modals2), 
   append(Literals1,Literals2,Literals), 
   append(Modals1,Modals2,Modals). 

snl(NewLiteral,A and B,N,Literals,Modals):- 
  snl_conjunction(NewLiteral,A and B,N,Literals,Modals). 

snl(NewLiteral,Formula,N,[],[Rule1,Rule2]):- 
   is_literal(Formula),!, 
   new_dontknow_prop(NewLiteral,Formula), 
   snff_negate(Formula, NegF), 
   snff_negate(NewLiteral, NegNewLiteral), 
   number_rule(NewLiteral imp neg k NegF,N,Rule1), 
   number_rule(NegNewLiteral imp k NegF,N,Rule2). 

/****************************************************************/ 
/* The following takes a formula and returns the corresponding  */
/* SNL definition clauses.                                      */
/****************************************************************/ 


snl_form(k L,[NegNewProp],[NewProp imp neg k L, NegNewProp imp k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(neg k L,[NewProp],[NewProp imp neg k L, NegNewProp imp k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(A or B,Literals,Modals):- 
   snl_form(A,Literals1,Modals1), 
   snl_form(B,Literals2,Modals2), 
   append(Literals1,Literals2,LiteralList), 
   disjoin(LiteralList,Literals), 
   append(Modals1,Modals2,Modals). 

/************************************************************/ 
/* The following deals with conjunctions of literals        */
/* and returns the new proposition renaming the conjunction */
/* as well as its corresponding definition clauses          */
/************************************************************/

/** Conjunctions of only snl literasl **/

snl_conjunction(SimpSNL,Conjunction,_,[],[]):-
  strip_and(Conjunction,List), 
  flatten(List,FList), 
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  subset(NewishList,[]), 
  conjoin(OldSNLList,OldSNL), 
  simplify_and(OldSNL,SimpSNL). 

/** conjunctions with at most one non snl-literal **/

snl_conjunction(NewConj,Conjunction,N,[],[Rule1,Rule2]):- 
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  test_conjunction(NewishList,Prop), 
  conjoin(OldSNLList,OldSNL), 
  new_dontknow_prop(NewSNL,Prop), 
  simplify_and(OldSNL and NewSNL,NewConj), 
  snff_negate(Prop,NegProp), 
  snff_negate(NewSNL,NegNewSNL), 
  number_rule(NewSNL imp neg k NegProp,N,Rule1), 
  number_rule(NegNewSNL imp k NegProp,N,Rule2). 

/** conjunction with more than one non snl literal **/

snl_conjunction(NewConj,Conjunction,N,Literals,SimplifiedRules):-
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  conjoin(OldSNLList,OldSNL),
  conjoin(NewishList,Formula),
  simplify_and(Formula,SFormula),
  new_conj_prop(NewProp,SFormula),
  snff_negate(SFormula,NFormula),
  number_rule(NewProp or NFormula,N,Definition),
  simplify([Definition],Literals),  
  new_dontknow_prop(NewSNL,NewProp),
  generate_new_modals(NewSNL,N,NewishList,NewModals),
  simplify_and(OldSNL and NewSNL,NewConj),
  snff_negate(NewProp,NegNewProp),
  snff_negate(NewSNL,NegNewSNL),
  number_rule(NewSNL imp neg k NegNewProp,N,Rule1),
  number_rule(NegNewSNL imp k NegNewProp,N,Rule2),
  append([Rule1],[Rule2],Definitions),
  append(Definitions,NewModals,Rules),
  simplify(Rules,SimplifiedRules).

/* generate new modals for conjunctions */

generate_new_modals(_,_,[],[]).

generate_new_modals(NewSNL,N,[Head|Tail],[Definition|NewModals]):-
  snff_negate(NewSNL,NegNewSNL),
  snff_negate(Head,NegHead),
  number_rule(NegNewSNL imp k NegHead,N,Definition),
  generate_new_modals(NewSNL,N,Tail,NewModals).

test_conjunction([Head|Tail],Head):- 
  subset(Tail,[]). 

/* takes a new proposition and returns its label */

remove_news([],[]). 

remove_news([new(X)|List],[X|NewList]):- 
  remove_news(List,NewList). 

remove_news([X|List],[X|NewList]):- 
  remove_news(List,NewList). 

/* remove SNL literals from a list of conjuncts */

separate_snl_literals(List,Literals,SNLLiterals):-
  sublist(is_snl_literal,List,SNLLiterals),
  subtract(List,SNLLiterals,Lit),
  sort(Lit,Literals).

/* My implementation of MRES5                    */

my_k_to_literal([],[]):- !. 

my_k_to_literal([r(N,_M,X imp k Y)|Rest],[Rule|NewRest]):-!, 
  snff_negate(X,NegX),
  number_rule(NegX or Y,[N,mres5],Rule),
  my_k_to_literal(Rest,NewRest).
 
my_k_to_literal([r(_,_,_ imp neg k _)|Rest],NewRest):-!, 
  my_k_to_literal(Rest,NewRest).
