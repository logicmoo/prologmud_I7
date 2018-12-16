% ectest/ec_reader_test_includes.e:1
% translate: begining  File: ectest/ec_reader_test_includes.e.pro 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/sorts.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:7
% sort rule,subject,object,action,ruleeffect,policy,policyset
sort([rule, subject, object, action, ruleeffect, policy, policyset]).


% 
% 
% 
% 
% 
% ectest/ec_reader_test_includes.e:13
% 
% 
% 
% 
% 
% 
% ectest/ec_reader_test_includes.e:19
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/RulesPatterns/ruleOutput.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:25
% fluent F_RuleDenied(rule)
fluent(f_ruleDenied(rule)).


% fluent F_RulePermitted(rule)
fluent(f_rulePermitted(rule)).


% 
% event Epermit(rule)
event(epermit(rule)).


% event EDeny(rule)
event(eDeny(rule)).


% 
% ectest/ec_reader_test_includes.e:31
% [rule,time] % Initiates(EDeny(rule),F_RuleDenied(rule),time).
initiates(eDeny(Rule), f_ruleDenied(Rule), Time).


% 
% ectest/ec_reader_test_includes.e:32
% [rule,time] % Initiates(Epermit(rule),F_RulePermitted(rule),time).
initiates(epermit(Rule), f_rulePermitted(Rule), Time).


% 
% 
% 
% ectest/ec_reader_test_includes.e:35
% [rule] % !HoldsAt(F_RulePermitted(rule),0).
not(holds_at(f_rulePermitted(Rule), 0)).


% 
% ectest/ec_reader_test_includes.e:36
% [rule] % !HoldsAt(F_RuleDenied(rule),0).
not(holds_at(f_ruleDenied(Rule), 0)).


% 
% 
% 
% 
% 
% 
% ectest/ec_reader_test_includes.e:42
% 
% 
% 
% 
% 
% 
% ectest/ec_reader_test_includes.e:48
% 
% ;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
% ;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/RulesPatterns/targetHolds.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:57
% fluent F_TargetHolds(rule)
fluent(f_targetHolds(rule)).


% fluent F_TargetDoesntHolds(rule)
fluent(f_targetDoesntHolds(rule)).


% 
% event E_MatchRuleParametters(rule)
event(e_matchRuleParametters(rule)).


% event E_DontMatchRuleParametters(rule)
event(e_dontMatchRuleParametters(rule)).


% 
% ectest/ec_reader_test_includes.e:63
% [rule,time] % Initiates(E_MatchRuleParametters(rule),F_TargetHolds(rule),time).
initiates(e_matchRuleParametters(Rule), f_targetHolds(Rule), Time).


% 
% ectest/ec_reader_test_includes.e:64
% [rule,time] % Initiates(E_DontMatchRuleParametters(rule),F_TargetDoesntHolds(rule),time).
initiates(e_dontMatchRuleParametters(Rule), f_targetDoesntHolds(Rule), Time).


% 
% 
% ectest/ec_reader_test_includes.e:66
% [rule,time] % Happens(E_MatchRuleParametters(rule), time) -> !HoldsAt(F_TargetHolds(rule),time).
happens(e_matchRuleParametters(Rule), Time) ->
	not(holds_at(f_targetHolds(Rule), Time)).


% 
% ectest/ec_reader_test_includes.e:67
% [rule,time] % Happens(E_DontMatchRuleParametters(rule), time) -> !HoldsAt(F_TargetDoesntHolds(rule),time).
happens(e_dontMatchRuleParametters(Rule), Time) ->
	not(holds_at(f_targetDoesntHolds(Rule), Time)).


% 
% 
% 
% ectest/ec_reader_test_includes.e:70
% [rule] % !HoldsAt(F_TargetHolds(rule),0).
not(holds_at(f_targetHolds(Rule), 0)).


% 
% ectest/ec_reader_test_includes.e:71
% [rule] % !HoldsAt(F_TargetDoesntHolds(rule),0).
not(holds_at(f_targetDoesntHolds(Rule), 0)).


% 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/RulesPatterns/ConditionsVerification.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:78
% fluent F_ConditionSatisfied(rule)
fluent(f_conditionSatisfied(rule)).


% ;event E_ConditionSatisfied(rule)
% 
% 
% ;[rule,time] Initiates(E_ConditionSatisfied(rule),F_ConditionSatisfied(rule),time).
% 
% ;[rule,time] Happens(E_ConditionSatisfied(rule),time) -> HoldsAt(F_TargetHolds(rule),time).
% ectest/ec_reader_test_includes.e:85
% 
% ;[rule,time] Happens(E_ConditionSatisfied(rule), time) -> !HoldsAt(F_ConditionSatisfied(rule),time).
% 
% 
% ectest/ec_reader_test_includes.e:89
% [rule] % HoldsAt(F_ConditionSatisfied(rule),0).
holds_at(f_conditionSatisfied(Rule), 0).


% 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/RulesPatterns/ruleModel.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:96
% fluent F_RuleEffectPermitted(rule)
fluent(f_ruleEffectPermitted(rule)).


% ; prédéfinies
% fluent F_RuleEffectNOTpermitted(rule) 
fluent(f_ruleEffectNOTpermitted(rule)).


% ;prédéfinies
% 
% 
% ectest/ec_reader_test_includes.e:102
% fluent F_RuleDenied(rule)
fluent(f_ruleDenied(rule)).


% fluent F_RulePermitted(rule)
fluent(f_rulePermitted(rule)).


% fluent F_RuleNotApplicable(rule)
fluent(f_ruleNotApplicable(rule)).


% 
% 
% 
% ectest/ec_reader_test_includes.e:108
% event Epermit(rule)
event(epermit(rule)).


% event EDeny(rule)
event(eDeny(rule)).


% event ERuleDoesNotApply(rule)
event(eRuleDoesNotApply(rule)).


% 
% 
% ectest/ec_reader_test_includes.e:113
% [rule,time] % Initiates(EDeny(rule),F_RuleDenied(rule),time).
initiates(eDeny(Rule), f_ruleDenied(Rule), Time).


% 
% ectest/ec_reader_test_includes.e:114
% [rule,time] % Initiates(Epermit(rule),F_RulePermitted(rule),time).
initiates(epermit(Rule), f_rulePermitted(Rule), Time).


% 
% ectest/ec_reader_test_includes.e:115
% [rule,time] % Initiates(ERuleDoesNotApply(rule),F_RuleNotApplicable(rule),time).
initiates(eRuleDoesNotApply(Rule), f_ruleNotApplicable(Rule), Time).


% 
% 
% 
% ectest/ec_reader_test_includes.e:118
% [rule,time] % Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
%                                             & HoldsAt(F_ConditionSatisfied(rule),time)
%                                             & HoldsAt(F_RuleEffectNOTpermitted(rule),time).
happens(eDeny(Rule), Time) ->
	holds_at(f_targetHolds(Rule), Time),
	holds_at(f_conditionSatisfied(Rule), Time),
	holds_at(f_ruleEffectNOTpermitted(Rule), Time).


% 
% 
% 
% ectest/ec_reader_test_includes.e:123
% [rule,time] % Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
%                                             & HoldsAt(F_ConditionSatisfied(rule),time)
%                                             & HoldsAt(F_RuleEffectPermitted(rule),time).
happens(epermit(Rule), Time) ->
	holds_at(f_targetHolds(Rule), Time),
	holds_at(f_conditionSatisfied(Rule), Time),
	holds_at(f_ruleEffectPermitted(Rule), Time).


% 
% 
% ectest/ec_reader_test_includes.e:127
% [rule,time] % Happens(ERuleDoesNotApply(rule),time) -> HoldsAt(F_TargetDoesntHolds(rule),time).
happens(eRuleDoesNotApply(Rule), Time) ->
	holds_at(f_targetDoesntHolds(Rule), Time).


% 
% 
% 
% 
% ectest/ec_reader_test_includes.e:131
% [rule] % !HoldsAt(F_RulePermitted(rule),0).
not(holds_at(f_rulePermitted(Rule), 0)).


% 
% ectest/ec_reader_test_includes.e:132
% [rule] % !HoldsAt(F_RuleDenied(rule),0).
not(holds_at(f_ruleDenied(Rule), 0)).


% 
% ectest/ec_reader_test_includes.e:133
% [rule] % !HoldsAt(F_RuleNotApplicable(rule),0).
not(holds_at(f_ruleNotApplicable(Rule), 0)).


% 
% 
% 
% 
% 
% ;********************************************************************************************************************
% ;--------------------------------------------------------------------------------------------------------------------
% ;********************************************************************************************************************
% ectest/ec_reader_test_includes.e:141
% 
% ;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
% 
% 
% 
% 
% ;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.
% ectest/ec_reader_test_includes.e:148
% 
% ;[rule,time,ruleeffect] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                                
% ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                                
% ;    & ruleeffect=Deny.
% 
% 
% ;[rule,time,ruleeffect] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                               
% ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                               
% ;    & ruleeffect=Permit.
% ectest/ec_reader_test_includes.e:157
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/ordering.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:163
% [rule,time] % Happens(E_MatchRuleParametters(rule), time) | Happens(E_DontMatchRuleParametters(rule), time) -> time = 0.
(   happens(e_matchRuleParametters(Rule), Time)
;   (   (   happens(e_matchRuleParametters(Rule), Time)
;   happens(e_dontMatchRuleParametters(Rule), Time)
->  Time=0
).


% 
% 
% ectest/ec_reader_test_includes.e:165
% [rule,time] % Happens(EDeny(rule), time) | Happens(Epermit(rule), time) | Happens(ERuleDoesNotApply(rule), time) -> time = 1.
(   happens(eDeny(Rule), Time)
;   happens(epermit(Rule), Time)
;   (   (   happens(eDeny(Rule), Time)
;   happens(epermit(Rule), Time)
;   happens(eRuleDoesNotApply(Rule), Time)
->  Time=1
).


% 
% 
% ;[policy,time] Happens(E_policyPermit(policy), time) | Happens(E_policyDeny(policy), time) | Happens(E_PolicyDoesNotApply(policy),time) -> time = 2.
% 
% 
% ;[policyset,time] Happens(E_policysetPermit(policyset), time) | Happens(E_policysetDeny(policyset), time) | Happens(E_policysetDontApply(policyset),time) -> time = 3.
% ectest/ec_reader_test_includes.e:171
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/PolicySetPatterns/policySetModel.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:177
% event E_policysetPermit(policyset)
event(e_policysetPermit(policyset)).


% event E_policysetDeny(policyset)
event(e_policysetDeny(policyset)).


% event E_policysetDontApply(policyset)
event(e_policysetDontApply(policyset)).


% 
% fluent F_policysetPermitted(policyset)
fluent(f_policysetPermitted(policyset)).


% fluent F_policysetDenied(policyset)
fluent(f_policysetDenied(policyset)).


% ectest/ec_reader_test_includes.e:183
% fluent F_policySetNotApplicable(policyset)
fluent(f_policySetNotApplicable(policyset)).


% 
% predicate PolicysetHaspolicies(policyset,policy)
predicate(policysetHaspolicies(policyset, policy)).


% 
% 
% ectest/ec_reader_test_includes.e:188
% [policyset,time] % Initiates(E_policysetPermit(policyset),F_policysetPermitted(policyset),time).
initiates(e_policysetPermit(Policyset), f_policysetPermitted(Policyset), Time).


% 
% ectest/ec_reader_test_includes.e:189
% [policyset,time] % Initiates(E_policysetDeny(policyset),F_policysetDenied(policyset),time).
initiates(e_policysetDeny(Policyset), f_policysetDenied(Policyset), Time).


% 
% ectest/ec_reader_test_includes.e:190
% [policyset,time] % Initiates(E_policysetDontApply(policyset),F_policySetNotApplicable(policyset),time).
initiates(e_policysetDontApply(Policyset), f_policySetNotApplicable(Policyset), Time).


% 
% 
% 
% 
% ; 'policies combaning algorithm (stategy) : All Permit'
% ectest/ec_reader_test_includes.e:195
% [policyset,policy,time] % Happens(E_policysetPermit(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyPermitted(policy),time).
happens(e_policysetPermit(Policyset), Time), policysetHaspolicies(Policyset, Policy) ->
	holds_at(f_policyPermitted(Policy), Time).


% 
% 
% 
% ; 'policies combaning algorithm (stategy) : Deny override'
% ectest/ec_reader_test_includes.e:199
% [policyset,time] % Happens(E_policysetDeny(policyset),time) -> {policy}  PolicysetHaspolicies(policyset,policy) & HoldsAt(F_policyDenied(policy),time).
happens(e_policysetDeny(Policyset), Time) ->
	exists([Policy],
	       (policysetHaspolicies(Policyset, Policy), holds_at(f_policyDenied(Policy), Time))).


% 
% 
% 
% ; 'policies combaning algorithm (stategy) : All Permit'
% ectest/ec_reader_test_includes.e:203
% [policyset,policy,time] % Happens(E_policysetDontApply(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyNotApplicable(policy),time).
happens(e_policysetDontApply(Policyset), Time), policysetHaspolicies(Policyset, Policy) ->
	holds_at(f_policyNotApplicable(Policy), Time).


% 
% 
% 
% ectest/ec_reader_test_includes.e:206
% [policyset]% !HoldsAt(F_policysetPermitted(policyset),0).
not(holds_at(f_policysetPermitted(Policyset), 0)).


% 
% ectest/ec_reader_test_includes.e:207
% [policyset]% !HoldsAt(F_policysetDenied(policyset),0).
not(holds_at(f_policysetDenied(Policyset), 0)).


% 
% ectest/ec_reader_test_includes.e:208
% [policyset]% !HoldsAt(F_policySetNotApplicable(policyset),0).
not(holds_at(f_policySetNotApplicable(Policyset), 0)).


% 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/PolicyPatterns/policyModel.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:215
% event E_policyPermit(policy)
event(e_policyPermit(policy)).


% event E_policyDeny(policy)
event(e_policyDeny(policy)).


% event E_PolicyDoesNotApply(policy)
event(e_policyDoesNotApply(policy)).


% 
% fluent F_policyPermitted(policy)
fluent(f_policyPermitted(policy)).


% fluent F_policyDenied(policy)
fluent(f_policyDenied(policy)).


% ectest/ec_reader_test_includes.e:221
% fluent F_policyNotApplicable(policy)
fluent(f_policyNotApplicable(policy)).


% 
% predicate PolicyHasRules(policy,rule)
predicate(policyHasRules(policy, rule)).


% 
% 
% ectest/ec_reader_test_includes.e:226
% [policy,time] % Initiates(E_policyPermit(policy),F_policyPermitted(policy),time).
initiates(e_policyPermit(Policy), f_policyPermitted(Policy), Time).


% 
% ectest/ec_reader_test_includes.e:227
% [policy,time] % Initiates(E_policyDeny(policy),F_policyDenied(policy),time).
initiates(e_policyDeny(Policy), f_policyDenied(Policy), Time).


% 
% ectest/ec_reader_test_includes.e:228
% [policy,time] % Initiates(E_PolicyDoesNotApply(policy),F_policyNotApplicable(policy),time).
initiates(e_policyDoesNotApply(Policy), f_policyNotApplicable(Policy), Time).


% 
% 
% 
% 
% ; 'Rule combaning algorithm (stategy) : All Permit'
% ectest/ec_reader_test_includes.e:233
% [policy,rule,time] % Happens(E_policyPermit(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RulePermitted(rule),time).
happens(e_policyPermit(Policy), Time), policyHasRules(Policy, Rule) ->
	holds_at(f_rulePermitted(Rule), Time).


% 
% 
% 
% ; 'Rule combaning algorithm (stategy) : Deny override (s il existe au moin une règle satisfaite)'
% ectest/ec_reader_test_includes.e:237
% [policy,time] % Happens(E_policyDeny(policy),time) -> {rule}  PolicyHasRules(policy,rule) & HoldsAt(F_RuleDenied(rule),time).
happens(e_policyDeny(Policy), Time) ->
	exists([Rule],
	       (policyHasRules(Policy, Rule), holds_at(f_ruleDenied(Rule), Time))).


% 
% 
% 
% ; 'Rule combaning algorithm (stategy) : All not Applicable'
% ectest/ec_reader_test_includes.e:241
% [policy,time,rule] % Happens(E_PolicyDoesNotApply(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RuleNotApplicable(rule),time).
happens(e_policyDoesNotApply(Policy), Time), policyHasRules(Policy, Rule) ->
	holds_at(f_ruleNotApplicable(Rule), Time).


% 
% 
% 
% ectest/ec_reader_test_includes.e:244
% [policy]% !HoldsAt(F_policyPermitted(policy),0).
not(holds_at(f_policyPermitted(Policy), 0)).


% 
% ectest/ec_reader_test_includes.e:245
% [policy]% !HoldsAt(F_policyDenied(policy),0).
not(holds_at(f_policyDenied(Policy), 0)).


% 
% ectest/ec_reader_test_includes.e:246
% [policy]% !HoldsAt(F_policyNotApplicable(policy),0).
not(holds_at(f_policyNotApplicable(Policy), 0)).


% 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: includes/SaaSPatterns/input.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_includes.e:253
% subject Navas
t(subject, navas).


% object Gloves
t(object, gloves).


% action Get
t(action, get).


% 
% ectest/ec_reader_test_includes.e:257
% translate: ending  File: ectest/ec_reader_test_includes.e.pro 
