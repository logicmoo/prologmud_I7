:- dynamic sentence/5.

sentence(A, B, E, C, G) :-
    declarative(A, B, D, C, F),
    terminator('.', D, E, F, G).
sentence(A, B, E, C, G) :-
    wh_question(A, B, D, C, F),
    terminator(?, D, E, F, G).
sentence(A, B, E, C, G) :-
    yn_question(A, B, D, C, F),
    terminator(?, D, E, F, G).
sentence(A, B, E, C, G) :-
    imperative(A, B, D, C, F),
    terminator(!, D, E, F, G).


:- dynamic declarative/5.

declarative(decl(A), B, C, D, E) :-
    s(A, _, B, C, D, E).


:- dynamic wh_question/5.

wh_question(whq(A, F), B, H, C, J) :-
    variable_q(A,
               _,
               D,
               E,
               B,
               G,
               C,
               I),
    question(D, E, F, G, H, I, J).


:- dynamic np/11.

np(B, C, D, E, F, G, H, A, A, I, J) :-
    virtual(np(B, C, D, E, F, G, H),
            I,
            J).
np(np(A, C, []), A, J, def, _, B, H, D, E, F, G) :-
    ix_is_pp(B),
    pers_pron(C, A, I, D, E, F, G),
    ix_empty(H),
    ix_role(I, decl, J).
np(np(A, C, E), A, _, D, I, B, L, F, N, G, P) :-
    ix_is_pp(B),
    np_head(C,
            A,
            D+H,
            J,
            E,
            F,
            M,
            G,
            O),
    ix_np_all(K),
    np_compls(H,
              A,
              I,
              J,
              K,
              L,
              M,
              N,
              O,
              P).
np(part(B, H), 3+C, _, indef, J, A, L, D, N, E, P) :-
    ix_is_pp(A),
    determiner(B, C, indef, D, F, E, G),
    terminal(of, F, M, G, O),
    ix_s_all(K),
    prep_case(I),
    np(H,
       3+plu,
       I,
       def,
       J,
       K,
       L,
       M,
       N,
       O,
       P).


:- dynamic variable_q/8.

variable_q(C, A, E, B, F, G, H, x(gap, nonterminal, np(D, A, B, _, _, J, K), I)) :-
    whq(C, A, D, E, F, G, H, I),
    ix_trace(J, K).
variable_q(D, E, compl, M, B, H, C, x(gap, nonterminal, pp(pp(A, F), compl, K, L), J)) :-
    prep(A, B, G, C, I),
    whq(D, E, F, _, G, H, I, J),
    ix_trace(K, L),
    compl_case(M).
variable_q(B, A, compl, K, E, F, G, x(gap, nonterminal, adv_phrase(pp(C, np(A, np_head(int_det(B), [], D), [])), I, J), H)) :-
    context_pron(C, D, E, F, G, H),
    ix_trace(I, J),
    verb_case(K).
variable_q(A, _, compl, J, B, F, C, x(gap, nonterminal, pred(adj, value(D, wh(A)), I), H)) :-
    terminal(how, B, E, C, G),
    adj(quant, D, E, F, G, H),
    ix_empty(I),
    verb_case(J).


:- dynamic pp/8.

pp(B, C, D, E, A, A, F, G) :-
    virtual(pp(B, C, D, E), F, G).
pp(pp(A, D), F, G, H, B, J, C, L) :-
    prep(A, B, I, C, K),
    prep_case(E),
    np(D,
       _,
       E,
       _,
       F,
       G,
       H,
       I,
       J,
       K,
       L).


:- dynamic adv_phrase/7.

adv_phrase(B, C, D, A, A, E, F) :-
    virtual(adv_phrase(B, C, D), E, F).
adv_phrase(pp(A, D), E, F, B, H, C, J) :-
    loc_pred(A, B, G, C, I),
    pp(pp(prep(of), D),
       compl,
       E,
       F,
       G,
       H,
       I,
       J).


:- dynamic pred/7.

pred(B, C, D, A, A, E, F) :-
    virtual(pred(B, C, D), E, F).
pred(_, A, B, C, D, E, F) :-
    adj_phrase(A, B, C, D, E, F).
pred(neg, A, C, D, E, F, G) :-
    ix_s_all(B),
    pp(A, compl, B, C, D, E, F, G).
pred(_, A, C, D, E, F, G) :-
    ix_s_all(B),
    adv_phrase(A, B, C, D, E, F, G).


:- dynamic whq/8.

whq(A, B, E, undef, C, H, D, J) :-
    int_det(A, B, C, G, D, I),
    ix_s_all(F),
    np(E,
       B,
       _,
       _,
       subj,
       F,
       _,
       G,
       H,
       I,
       J).
whq(B, 3+A, np(3+A, wh(B), []), C, D, E, F, G) :-
    int_pron(C, D, E, F, G).


:- dynamic int_det/6.

int_det(A, 3+B, C, D, E, F) :-
    whose(A, B, C, D, E, F).
int_det(A, 3+B, C, D, E, F) :-
    int_art(A, B, C, D, E, F).


:- dynamic np_head0/7.

np_head0(B, C, D, A, A, E, F) :-
    virtual(np_head0(B, C, D), E, F).
np_head0(name(A), 3+sg, def+proper, B, C, D, E) :-
    name(A, B, C, D, E).
np_head0(np_head(A, F, I), 3+B, C+common, D, K, E, M) :-
    determiner(A, B, C, D, G, E, H),
    adjs(F, G, J, H, L),
    noun(I, B, J, K, L, M).
np_head0(A, B, def+proper, C, D, E, x(nogap, nonterminal, gen_marker, F)) :-
    poss_pron(A, B, C, D, E, F).
np_head0(np_head(A, [], B), 3+sg, indef+common, C, D, E, F) :-
    quantifier_pron(A, B, C, D, E, F).


:- dynamic gen_marker/4.

gen_marker(A, A, B, C) :-
    virtual(gen_marker, B, C).
gen_marker(A, D, B, F) :-
    terminal('\'', A, C, B, E),
    an_s(C, D, E, F).


:- dynamic whose/6.

whose(A, B, C, D, E, x(nogap, nonterminal, np_head0(wh(A), B, proper), x(nogap, nonterminal, gen_marker, F))) :-
    terminal(whose, C, D, E, F).


:- dynamic question/7.

question(A, B, C, D, E, F, G) :-
    subj_question(A),
    ix_role(subj, _, B),
    s(C, _, D, E, F, G).
question(A, B, E, C, G, D, I) :-
    fronted_verb(A, B, C, F, D, H),
    s(E, _, F, G, H, I).


:- dynamic det/7.

det(B, C, D, A, A, E, F) :-
    virtual(det(B, C, D), E, F).
det(det(G), F, H, A, B, C, D) :-
    terminal(E, A, B, C, D),
    det(E, F, G, H).
det(generic, _, generic, A, A, B, B).


:- dynamic int_art/6.

int_art(B, A, D, E, F, x(nogap, nonterminal, det(C, A, def), G)) :-
    int_art(B, A, C, D, E, F, G).


:- dynamic subj_qustion/1.

subj_qustion(subj).


:- dynamic subj_question/1.

subj_question(undef).


:- dynamic yn_question/5.

yn_question(q(C), A, E, B, G) :-
    fronted_verb(nil, _, A, D, B, F),
    s(C, _, D, E, F, G).


:- dynamic verb_form/8.

verb_form(B, C, D, E, A, A, F, G) :-
    virtual(verb_form(B, C, D, E), F, G).
verb_form(F, G, H, _, A, B, C, D) :-
    terminal(E, A, B, C, D),
    verb_form(E, F, G, H).


:- dynamic neg/6.

neg(B, C, A, A, D, E) :-
    virtual(neg(B, C), D, E).
neg(aux+_, neg, A, B, C, D) :-
    terminal(not, A, B, C, D).
neg(_, pos, A, A, B, B).


:- dynamic fronted_verb/6.

fronted_verb(F, H, D, K, E, x(gap, nonterminal, verb_form(A, B, C, G), x(nogap, nonterminal, neg(_, I), M))) :-
    verb_form(A,
              B,
              C,
              _,
              D,
              J,
              E,
              L),
    verb_type(A, aux+_),
    ix_role(F, G, H),
    neg(_, I, J, K, L, M).


:- dynamic imperative/5.

imperative(imp(C), A, E, B, G) :-
    imperative_verb(A, D, B, F),
    s(C, _, D, E, F, G).


:- dynamic imperative_verb/4.

imperative_verb(B, C, D, x(nogap, terminal, you, x(nogap, nonterminal, verb_form(A, imp+fin, 2+sg, main), E))) :-
    verb_form(A, inf, _, _, B, C, D, E).


:- dynamic s/6.

s(s(A, D, J, P), S, B, U, C, W) :-
    subj(A, E, F, B, G, C, H),
    verb(D, E, F, I, G, L, H, M),
    ix_empty(K),
    ix_s_all(N),
    verb_args(F,
              I,
              J,
              K,
              O,
              L,
              T,
              M,
              V),
    ix_minus(N, O, Q),
    ix_plus(N, O, R),
    verb_mods(P,
              Q,
              R,
              S,
              T,
              U,
              V,
              W).


:- dynamic subj/7.

subj(there, _, _+be, A, B, C, D) :-
    terminal(there, A, B, C, D).
subj(A, B, _, E, F, G, H) :-
    ix_s_all(D),
    subj_case(C),
    np(A,
       B,
       C,
       _,
       subj,
       D,
       _,
       E,
       F,
       G,
       H).


:- dynamic np_head/9.

np_head(G, H, I, J, K, A, M, B, O) :-
    np_head0(C, D, E, A, L, B, N),
    possessive(C,
               D,
               E,
               F,
               F,
               G,
               H,
               I,
               J,
               K,
               L,
               M,
               N,
               O).


:- dynamic np_compls/10.

np_compls(proper, _, _, [], _, C, A, A, B, B) :-
    ix_empty(C).
np_compls(common, A, B, C, D, K, F, M, G, O) :-
    ix_np_all(E),
    np_mods(A,
            B,
            H,
            C,
            D,
            I,
            E,
            J,
            F,
            L,
            G,
            N),
    relative(A,
             H,
             I,
             J,
             K,
             L,
             M,
             N,
             O).


:- dynamic possessive/14.

possessive(I, H, _, [], J, L, M, N, O, P, A, R, B, T) :-
    gen_case(A, C, B, D),
    np_head0(E, F, G, C, Q, D, S),
    possessive(E,
               F,
               G,
               K,
               [pp(poss, np(H, I, J))|K],
               L,
               M,
               N,
               O,
               P,
               Q,
               R,
               S,
               T).
possessive(A, B, C, D, E, A, B, C, D, E, F, F, G, G).


:- dynamic gen_case/4.

gen_case(A, B, C, x(nogap, terminal, the, D)) :-
    gen_marker(A, B, C, D).


:- dynamic an_s/4.

an_s(A, B, C, D) :-
    terminal(s, A, B, C, D).
an_s(A, A, B, B).


:- dynamic determiner/7.

determiner(A, B, C, D, E, F, G) :-
    det(A, B, C, D, E, F, G).
determiner(A, B, C, D, E, F, G) :-
    quant_phrase(A, B, C, D, E, F, G).


:- dynamic quant_phrase/7.

quant_phrase(quant(A, E), F, B, C, H, D, J) :-
    quant(A, B, C, G, D, I),
    number(E, F, G, H, I, J).


:- dynamic quant/6.

quant(A, indef, B, H, C, J) :-
    neg_adv(D, A, B, E, C, F),
    comp_adv(D, E, G, F, I),
    terminal(than, G, H, I, J).
quant(H, indef, A, D, B, F) :-
    terminal(at, A, C, B, E),
    sup_adv(G, C, D, E, F),
    sup_op(G, H).
quant(the, def, A, B, C, D) :-
    terminal(the, A, B, C, D).
quant(same, indef, A, A, B, B).


:- dynamic neg_adv/6.

neg_adv(A, not+A, B, C, D, E) :-
    terminal(not, B, C, D, E).
neg_adv(A, A, B, B, C, C).


:- dynamic sup_op/2.

sup_op(least, not+less).
sup_op(most, not+more).


:- dynamic np_mods/12.

np_mods(A, B, J, [C|K], D, M, _, O, E, Q, F, S) :-
    np_mod(A,
           B,
           C,
           D,
           H,
           E,
           P,
           F,
           R),
    ix_trace(G),
    ix_plus(G, H, I),
    ix_minus(D, I, L),
    ix_plus(H, D, N),
    np_mods(A,
            B,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S).
np_mods(_, _, A, A, B, B, C, C, D, D, E, E).


:- dynamic np_mod/9.

np_mod(_, B, A, C, D, E, F, G, H) :-
    pp(A, B, C, D, E, F, G, H).
np_mod(A, _, B, C, D, E, F, G, H) :-
    reduced_relative(A,
                     B,
                     C,
                     D,
                     E,
                     F,
                     G,
                     H).


:- dynamic verb_mods/8.

verb_mods([A|H], B, _, K, C, M, D, O) :-
    verb_mod(A, B, F, C, L, D, N),
    ix_trace(E),
    ix_plus(E, F, G),
    ix_minus(B, G, I),
    ix_plus(F, B, J),
    verb_mods(H,
              I,
              J,
              K,
              L,
              M,
              N,
              O).
verb_mods([], _, A, A, B, B, C, C).


:- dynamic verb_mod/7.

verb_mod(A, B, C, D, E, F, G) :-
    adv_phrase(A, B, C, D, E, F, G).
verb_mod(B, A, G, C, D, E, F) :-
    ix_is_adv(A),
    adverb(B, C, D, E, F),
    ix_empty(G).
verb_mod(A, B, C, D, E, F, G) :-
    pp(A, compl, B, C, D, E, F, G).


:- dynamic adjs/5.

adjs([A|D], B, F, C, H) :-
    pre_adj(A, B, E, C, G),
    adjs(D, E, F, G, H).
adjs([], A, A, B, B).


:- dynamic pre_adj/5.

pre_adj(A, B, C, D, E) :-
    adj(_, A, B, C, D, E).
pre_adj(A, B, C, D, E) :-
    sup_phrase(A, B, C, D, E).


:- dynamic sup_phrase/5.

sup_phrase(sup(most, A), B, C, D, E) :-
    sup_adj(A, B, C, D, E).
sup_phrase(sup(_, C), A, E, B, G) :-
    sup_adv(_, A, D, B, F),
    adj(quant, C, D, E, F, G).


:- dynamic comp_phrase/6.

comp_phrase(comp(A, B, E), H, C, J, D, L) :-
    comp(A, B, C, I, D, K),
    ix_np_no_trace(G),
    prep_case(F),
    np(E,
       _,
       F,
       _,
       compl,
       G,
       H,
       I,
       J,
       K,
       L).


:- dynamic comp/6.

comp(A, D, B, H, C, J) :-
    comp_adv(A, B, E, C, F),
    adj(quant, D, E, G, F, I),
    terminal(than, G, H, I, J).
comp(more, A, B, E, C, G) :-
    rel_adj(A, B, D, C, F),
    terminal(than, D, E, F, G).
comp(same, C, A, G, B, I) :-
    terminal(as, A, D, B, E),
    adj(quant, C, D, F, E, H),
    terminal(as, F, G, H, I).


:- dynamic relative/9.

relative(B, [C], A, _, D, E, F, G, H) :-
    ix_is_pred(A),
    rel_conj(B, _, C, D, E, F, G, H).
relative(_, [], _, A, A, B, B, C, C).


:- dynamic rel_conj/8.

rel_conj(A, D, F, H, B, J, C, L) :-
    rel(A, E, G, B, I, C, K),
    rel_rest(A,
             D,
             E,
             F,
             G,
             H,
             I,
             J,
             K,
             L).


:- dynamic rel_rest/10.

rel_rest(F, A, B, C, _, I, D, K, E, M) :-
    conj(A,
         G,
         B,
         H,
         C,
         D,
         J,
         E,
         L),
    rel_conj(F,
             G,
             H,
             I,
             J,
             K,
             L,
             M).
rel_rest(_, _, A, A, B, B, C, C, D, D).


:- dynamic rel/7.

rel(C, rel(D, G), L, A, N, B, P) :-
    island(A, E, B, F),
    variable(C, D, E, H, F, I),
    s(G, J, H, M, I, O),
    ix_trace(K),
    ix_minus(J, K, L),
    dnalsi(M, N, O, P).


:- dynamic variable/6.

variable(A, B, C, D, E, x(gap, nonterminal, np(np(A, wh(B), []), A, _, _, _, G, H), F)) :-
    terminal(that, C, D, E, F),
    ix_trace(G, H).
variable(B, A, F, G, H, x(gap, nonterminal, np(C, D, E, _, _, J, K), I)) :-
    wh(A,
       B,
       C,
       D,
       E,
       F,
       G,
       H,
       I),
    ix_trace(J, K).
variable(E, D, B, H, C, x(gap, nonterminal, pp(pp(A, F), compl, K, L), J)) :-
    prep(A, B, G, C, I),
    wh(D,
       E,
       F,
       _,
       M,
       G,
       H,
       I,
       J),
    ix_trace(K, L),
    compl_case(M).


:- dynamic wh/9.

wh(B, A, np(A, wh(B), []), A, H, C, D, E, F) :-
    rel_pron(G, C, D, E, F),
    ix_role(G, decl, H).
wh(H, I, np(A, B, [pp(E, J)]), A, _, C, L, D, N) :-
    np_head0(B, A, _+common, C, F, D, G),
    prep(E, F, K, G, M),
    wh(H,
       I,
       J,
       _,
       _,
       K,
       L,
       M,
       N).
wh(A, B, E, F, G, C, J, D, L) :-
    whose(A, B, C, I, D, K),
    ix_s_all(H),
    np(E,
       F,
       G,
       def,
       subj,
       H,
       _,
       I,
       J,
       K,
       L).


:- dynamic reduced_relative/8.

reduced_relative(B, C, A, D, E, F, G, H) :-
    ix_is_pred(A),
    reduced_rel_conj(B,
                     _,
                     C,
                     D,
                     E,
                     F,
                     G,
                     H).


:- dynamic reduced_rel_conj/8.

reduced_rel_conj(A, D, F, H, B, J, C, L) :-
    reduced_rel(A, E, G, B, I, C, K),
    reduced_rel_rest(A,
                     D,
                     E,
                     F,
                     G,
                     H,
                     I,
                     J,
                     K,
                     L).


:- dynamic reduced_rel_rest/10.

reduced_rel_rest(F, A, B, C, _, I, D, K, E, M) :-
    conj(A,
         G,
         B,
         H,
         C,
         D,
         J,
         E,
         L),
    reduced_rel_conj(F,
                     G,
                     H,
                     I,
                     J,
                     K,
                     L,
                     M).
reduced_rel_rest(_, _, A, A, B, B, C, C, D, D).


:- dynamic reduced_rel/7.

reduced_rel(C, reduced_rel(D, G), L, A, N, B, P) :-
    island(A, E, B, F),
    reduced_wh(C, D, E, H, F, I),
    s(G, J, H, M, I, O),
    ix_trace(K),
    ix_minus(J, K, L),
    dnalsi(M, N, O, P).


:- dynamic reduced_wh/6.

reduced_wh(A, B, D, I, E, x(nogap, nonterminal, np(np(A, wh(B), []), A, N, _, _, L, M), x(nogap, nonterminal, verb_form(be, pres+fin, A, main), x(nogap, nonterminal, neg(_, C), x(nogap, nonterminal, pred(C, F, G), K))))) :-
    neg(_, C, D, H, E, J),
    pred(C, F, G, H, I, J, K),
    ix_trace(L, M),
    subj_case(N).
reduced_wh(A, B, F, G, H, x(nogap, nonterminal, np(np(A, wh(B), []), A, L, _, _, J, K), x(nogap, nonterminal, verb(C, _, D, E), I))) :-
    participle(C, D, E, F, G, H, I),
    ix_trace(J, K),
    subj_case(L).
reduced_wh(A, B, I, J, K, x(nogap, nonterminal, np(E, F, C, G, _, M, N), x(gap, nonterminal, np(np(A, wh(B), []), A, D, _, _, O, P), L))) :-
    ix_s_all(H),
    subj_case(C),
    verb_case(D),
    np(E,
       F,
       _,
       G,
       subj,
       H,
       _,
       I,
       J,
       K,
       L),
    ix_trace(M, N),
    ix_trace(O, P).


:- dynamic verb/8.

verb(B, C, D, E, A, A, F, G) :-
    virtual(verb(B, C, D, E), F, G).
verb(verb(L, A, B+fin, M, H), C, R, A, D, O, E, Q) :-
    verb_form(F,
              B+fin,
              C,
              K,
              D,
              I,
              E,
              J),
    verb_type(F, G),
    neg(G, H, I, N, J, P),
    rest_verb(K,
              F,
              L,
              A,
              M,
              N,
              O,
              P,
              Q),
    verb_type(L, R).


:- dynamic rest_verb/9.

rest_verb(aux, have, D, E, [perf|F], A, H, B, J) :-
    verb_form(C,
              past+part,
              _,
              _,
              A,
              G,
              B,
              I),
    have(C, D, E, F, G, H, I, J).
rest_verb(aux, be, E, F, G, A, I, B, K) :-
    verb_form(D, C, _, _, A, H, B, J),
    be(C,
       D,
       E,
       F,
       G,
       H,
       I,
       J,
       K).
rest_verb(aux, do, A, active, [], B, C, D, E) :-
    verb_form(A, inf, _, _, B, C, D, E).
rest_verb(main, A, A, active, [], B, B, C, C).


:- dynamic have/8.

have(be, E, F, G, A, I, B, K) :-
    verb_form(D, C, _, _, A, H, B, J),
    be(C,
       D,
       E,
       F,
       G,
       H,
       I,
       J,
       K).
have(A, A, active, [], B, B, C, C).


:- dynamic be/9.

be(past+part, A, A, passive, [], B, B, C, C).
be(pres+part, A, B, C, [prog], D, E, F, G) :-
    passive(A, B, C, D, E, F, G).


:- dynamic passive/7.

passive(be, A, passive, B, C, D, E) :-
    verb_form(A,
              past+part,
              _,
              _,
              B,
              C,
              D,
              E),
    verb_type(A, F),
    passive(F).
passive(A, A, active, B, B, C, C).


:- dynamic participle/7.

participle(verb(E, A, inf, K, B), L, A, C, G, D, I) :-
    neg(_, B, C, F, D, H),
    verb_form(E, J, _, _, F, G, H, I),
    participle(J, A, K),
    verb_type(E, L).


:- dynamic passive/1.

passive(_+trans).
passive(_+ditrans).


:- dynamic participle/3.

participle(pres+part, active, [prog]).
participle(past+part, passive, []).


:- dynamic dnalsi/4.

dnalsi(A, A, B, C) :-
    virtual(dnalsi, B, C).


:- dynamic island/4.

island(A, A, B, x(gap, nonterminal, dnalsi, B)).


:- dynamic verb_args/9.

verb_args(_+D, E, A, G, H, B, J, C, L) :-
    advs(A, F, _, B, I, C, K),
    verb_args(D,
              E,
              F,
              G,
              H,
              I,
              J,
              K,
              L).
verb_args(trans, active, [arg(dir, A)], _, B, C, D, E, F) :-
    verb_arg(np, A, B, C, D, E, F).
verb_args(ditrans, _, [arg(D, A)|E], _, G, B, I, C, K) :-
    verb_arg(np, A, F, B, H, C, J),
    object(D, E, F, G, H, I, J, K).
verb_args(be, _, [void], A, A, B, C, D, E) :-
    terminal(there, B, C, D, E).
verb_args(be, _, [arg(pred, A)], _, B, C, D, E, F) :-
    pred_conj(_, A, B, C, D, E, F).
verb_args(be, _, [arg(dir, A)], _, B, C, D, E, F) :-
    verb_arg(np, A, B, C, D, E, F).
verb_args(have, active, [arg(dir, A)], _, B, C, D, E, F) :-
    verb_arg(np, A, B, C, D, E, F).
verb_args(D, _, [], A, A, B, B, C, C) :-
    no_args(D).


:- dynamic object/8.

object(G, C, B, I, E, K, F, M) :-
    ix_adv(A),
    ix_minus(A, B, D),
    advs(C, H, D, E, J, F, L),
    obj(G, H, B, I, J, K, L, M).


:- dynamic obj/8.

obj(ind, [arg(dir, A)], _, B, C, D, E, F) :-
    verb_arg(np, A, B, C, D, E, F).
obj(dir, [], A, A, B, B, C, C).


:- dynamic pred_conj/7.

pred_conj(C, E, G, A, I, B, K) :-
    pred(_, D, F, A, H, B, J),
    pred_rest(C,
              D,
              E,
              F,
              G,
              H,
              I,
              J,
              K).


:- dynamic pred_rest/9.

pred_rest(A, B, C, _, H, D, J, E, L) :-
    conj(A,
         F,
         B,
         G,
         C,
         D,
         I,
         E,
         K),
    pred_conj(F, G, H, I, J, K, L).
pred_rest(_, A, A, B, B, C, C, D, D).


:- dynamic verb_arg/7.

verb_arg(np, A, D, E, F, G, H) :-
    ix_s_all(C),
    verb_case(B),
    np(A,
       _,
       B,
       _,
       compl,
       C,
       D,
       E,
       F,
       G,
       H).


:- dynamic advs/7.

advs([B|E], F, A, C, H, D, J) :-
    ix_is_adv(A),
    adverb(B, C, G, D, I),
    advs(E, F, A, G, H, I, J).
advs(A, A, _, B, B, C, C).


:- dynamic adj_phrase/6.

adj_phrase(A, F, B, C, D, E) :-
    adj(_, A, B, C, D, E),
    ix_empty(F).
adj_phrase(A, B, C, D, E, F) :-
    comp_phrase(A, B, C, D, E, F).


:- dynamic no_args/1.

no_args(trans).
no_args(ditrans).
no_args(intrans).


:- dynamic conj/9.

conj(conj(A, D), conj(A, E), B, C, conj(A, B, C), F, G, H, I) :-
    conj(A, D, E, F, G, H, I).


:- dynamic noun/6.

noun(F, G, A, B, C, D) :-
    terminal(E, A, B, C, D),
    noun_form(E, F, G).


:- dynamic adj/6.

adj(F, adj(A), B, C, D, E) :-
    terminal(A, B, C, D, E),
    adj(A, F).


:- dynamic prep/5.

prep(prep(A), B, C, D, E) :-
    terminal(A, B, C, D, E),
    prep(A).


:- dynamic rel_adj/5.

rel_adj(adj(F), A, B, C, D) :-
    terminal(E, A, B, C, D),
    rel_adj(E, F).


:- dynamic sup_adj/5.

sup_adj(adj(F), A, B, C, D) :-
    terminal(E, A, B, C, D),
    sup_adj(E, F).


:- dynamic comp_adv/5.

comp_adv(less, A, B, C, D) :-
    terminal(less, A, B, C, D).
comp_adv(more, A, B, C, D) :-
    terminal(more, A, B, C, D).


:- dynamic sup_adv/5.

sup_adv(least, A, B, C, D) :-
    terminal(least, A, B, C, D).
sup_adv(most, A, B, C, D) :-
    terminal(most, A, B, C, D).


:- dynamic rel_pron/5.

rel_pron(F, A, B, C, D) :-
    terminal(E, A, B, C, D),
    rel_pron(E, F).


:- dynamic name/5.

name(C, A, E, B, G) :-
    opt_the(A, D, B, F),
    terminal(C, D, E, F, G),
    name(C).


:- dynamic int_art/7.

int_art(A, plu, quant(same, wh(A)), B, E, C, G) :-
    terminal(how, B, D, C, F),
    terminal(many, D, E, F, G).
int_art(F, G, H, A, B, C, D) :-
    terminal(E, A, B, C, D),
    int_art(E, F, G, H).


:- dynamic int_pron/5.

int_pron(F, A, B, C, D) :-
    terminal(E, A, B, C, D),
    int_pron(E, F).


:- dynamic adverb/5.

adverb(adv(A), B, C, D, E) :-
    terminal(A, B, C, D, E),
    adverb(A).


:- dynamic poss_pron/6.

poss_pron(pronoun(F), G+H, A, B, C, D) :-
    terminal(E, A, B, C, D),
    poss_pron(E, F, G, H).


:- dynamic pers_pron/7.

pers_pron(pronoun(F), G+H, I, A, B, C, D) :-
    terminal(E, A, B, C, D),
    pers_pron(E, F, G, H, I).


:- dynamic quantifier_pron/6.

quantifier_pron(F, G, A, B, C, D) :-
    terminal(E, A, B, C, D),
    quantifier_pron(E, F, G).


:- dynamic context_pron/6.

context_pron(prep(in), place, A, B, C, D) :-
    terminal(where, A, B, C, D).
context_pron(prep(at), time, A, B, C, D) :-
    terminal(when, A, B, C, D).


:- dynamic number/6.

number(nb(F), G, A, B, C, D) :-
    terminal(E, A, B, C, D),
    number(E, F, G).


:- dynamic terminator/5.

terminator(F, A, B, C, D) :-
    terminal(E, A, B, C, D),
    terminator(E, F).


:- dynamic opt_the/4.

opt_the(A, A, B, B).
opt_the(A, B, C, D) :-
    terminal(the, A, B, C, D).


:- dynamic conj/7.

conj(_, list, list, A, B, C, D) :-
    terminal(',', A, B, C, D).
conj(A, list, end, B, C, D, E) :-
    terminal(A, B, C, D, E),
    conj(A).


:- dynamic loc_pred/5.

loc_pred(F, A, B, C, D) :-
    terminal(E, A, B, C, D),
    loc_pred(E, F).


