:- use_module(preprocessing).
:- use_module(tokenizer).
:- use_module(sr_parser).

:- begin_tests(test).

test(lower_string):-
    preprocessing('UPPERCASE Test STRINg', X),
    name(Output, X),
    assertion(Output == 'uppercase test string').

test(clean_special_characters):-
    preprocessing('!!?cle$ani...ng ,,,,,th))(e)( s/&tr"ing-', X),
    name(Output, X),
    assertion(Output == 'cleaning the string').

test(normalize_multiple_spaces):-
    preprocessing('too              many             spaces', X),
    name(Output, X),
    assertion(Output == 'too many spaces').

test(preprocessing_all):-
    preprocessing('I (shot),,, -MARViN .in.         the$ & FACE!?', X),
    name(Output, X),
    assertion(Output == 'i shot marvin in the face').

test(tokenizer):-
    name('the rain in spain stays mainly in the plain', X),
    tokenize(X, Tokens),
    assertion(Tokens == [the,rain,in,spain,stays,mainly,in,the,plain]).

test(prepocessed_tokenizer):-
    preprocessing('The Man shot the Woman!', L),
    tokenize(L, Tokens),
    assertion(Tokens == [the,man,shot,the,woman]).

test(parser_simple, nondet):-
    sr_parse([], [amy,plays,piano], tp(tp(dp(np(n(amy))),t1(vp(v(plays),dp(np(n(piano)))))))).

test(parser_question, nondet):-
    sr_parse([], [what,does,alice,play,at,school], cp(dp(det(what)),c1(c(does),tp(dp(np(n(alice))),t1(vp(vp(v(play)),ppP(pp(at),dp(np(n(school)))))))))).

test(preprocessed_tokenized_parser, nondet):-
    preprocessing('Does Kevin bake a pie???', L),
    tokenize(L, Token),
    sr_parse([], Token, Tree),
    assertion(Tree == c1(c1(c(does),tp(dp(np(n(kevin))),t1(vp(v(bake),dp(det(a),np(n(pie))))))))).

:- end_tests(test).




