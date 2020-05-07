:- module(sr_parser, 
    [sr_parse/3]).

:- op(1100, xfy, '==>').
:- op(1100, xfy, '=>').

% Lexicon
n(n(dog)) => [dog].
n(n(dogs)) => [dogs].
n(n(bone)) => [bone].
n(n(bones)) => [bones].
n(n(night)) => [night].
n(n(nights)) => [nights].
n(n(village)) => [village].
n(n(villages)) => [villages].
n(n(cat)) => [cat].
n(n(cats)) => [cats].
n(n(alice)) => [alice].
n(n(amy)) => [amy].
n(n(pie)) => [pie].
n(n(piano)) => [piano].
n(n(kevin)) => [kevin].
n(n(watchmen)) => [watchmen].
n(n(box)) => [box].
n(n(school)) => [school].
n(n(hat)) => [hat].

v(v(bark)) => [bark].
v(v(eat)) => [eat].
v(v(chase)) => [chase].
v(v(play)) => [play].
v(v(laugh)) => [laugh].
v(v(like)) => [like].
v(v(shoot)) => [shoot].
v(v(watch)) => [watch].
v(v(bake)) => [bake].
v(v(open)) => [open].
v(v(get)) => [get].

v(v(plays)) => [plays].
v(v(laughs)) => [laughs].
v(v(bakes)) => [bakes].
v(v(chases)) => [chases].
v(v(likes)) => [likes].
v(v(barks)) => [barks].
v(v(eats)) => [eats].
v(v(shoots)) => [shoots].
v(v(watches)) => [watches].

v(v(barking)) => [barking].
v(v(eating)) => [eating].
v(v(chasing)) => [chasing].
v(v(playing)) => [playing].
v(v(laughing)) => [laughing].
v(v(baking)) => [baking].
v(v(shooting)) => [shooting].
v(v(watching)) => [watching].

adj(adj(big)) => [big].
adj(adj(black)) => [black].
adj(adj(furry)) => [furry].

adv(adv(quickly)) => [quickly].
adv(adv(where)) => [where].
adv(adv(when)) => [when].
adv(adv(why)) => [why].
adv(adv(how)) => [how].

pp(pp(at)) => [at].
pp(pp(in)) => [in].
pp(pp(on)) => [on].

det(det(the)) => [the].
det(det(a)) => [a].
det(det(what)) => [what].
det(det(who)) => [who].
det(det(that)) => [that].

c(c(is)) => [is].
c(c(are)) => [are].
c(c(do)) => [do].
c(c(does)) => [does].
c(c(did)) => [did].

t(t(is)) => [is].
t(t(are)) => [are].
t(t(do)) => [do].
t(t(does)) => [does].
t(t(did)) => [did].
t(t(can)) => [can].
t(t(would)) => [would].
t(t(should)) => [should].

pro(pro(i)) => [i].
pro(pro(you)) => [you].
pro(pro(he)) => [he].
pro(pro(she)) => [she].
pro(pro(it)) => [it].
pro(pro(they)) => [they].
pro(pro(we)) => [we].
pro(pro(him)) => [him].
pro(pro(her)) => [her].
pro(pro(them)) => [them].
pro(pro(us)) => [us].
pro(pro(me)) => [me].


% Phrase Structure Rules
cp(DP, C1) ==> dp(DP), c1(C1).
cp(ADV, C1) ==> advP(ADV), c1(C1).

c1(c1(C, TP)) ==> c(C), tp(TP).
c1(c1(TP)) ==> tp(TP).

tp(tp(DP, T1)) ==> dp(DP), t1(T1).
tp(tp(PP)) ==> ppP(PP).

t1(t1(T, VP)) ==> t(T), vp(VP).
t1(t1(T, ADJP)) ==> t(T), adjP(ADJP).
t1(t1(T, DP)) ==> t(T), dp(DP).
t1(t1(VP)) ==> vp(VP).

np(np(N)) ==> n(N).
np(np(N)) ==> pro(N).
np(np(ADJ, N)) ==> adjP(ADJ), np(N).
np(np(N, P)) ==> np(N), ppP(P).

adjP(adjP(ADJ)) ==>  adj(ADJ).

vp(vp(V)) ==> v(V).
vp(vp(V, NP)) ==> v(V), np(NP).
vp(vp(V, DP)) ==> v(V), dp(DP).
vp(vp(V, ADV)) ==> vp(V), advP(ADV).
vp(vp(ADV, V)) ==> advP(ADV), vp(V).
vp(vp(V, P)) ==> vp(V), ppP(P).

advP(advP(ADV)) ==> adv(ADV).

ppP(ppP(P, N)) ==> pp(P), np(N).
ppP(ppP(P, DP)) ==> pp(P), dp(DP).

dp(dp(D, NP)) ==> det(D), np(NP).
dp(dp(NP)) ==> np(NP).
dp(dp(D)) ==> det(D).


%%% Shift-Reduce Parser

% Base
sr_parse([C], [], C).

% Shift
sr_parse(Stack, [Word|Words], C):-
    (Cat => [Word]),
    sr_parse([Cat|Stack], Words, C).

% Reduce
sr_parse([X|Rest], String, C):-
    (Y ==> X),
    sr_parse([Y|Rest], String, C).
sr_parse([Y,X|Rest], String, C):-
    (Z ==> X, Y),
    sr_parse([Z|Rest], String, C).
