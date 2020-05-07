:- module(preprocessing, [preprocessing/2]).
% get rid of uppercase letter, special characters, punctuations and multiple spaces
%%% ASCII Characters
% 46 -> . 63 -> ? 44 -> , 33 -> ! 34 -> "
% 38 -> & 40 -> ( 41 -> ) 45 -> - 47 -> /
preprocessing(S, X) :-
    string_lower(S, LowerCase),
    name(LowerCase, ASCIIList),
    exclude([X]>>(X = 46; X = 44; X = 63; X = 33; X = 34; X = 36; X = 38; X = 40; X = 41; X = 45; X = 47), ASCIIList, NewASCIIList),
    name(Words, NewASCIIList),
    normalize_space(atom(Y), Words),
    name(Y, X).


    
    