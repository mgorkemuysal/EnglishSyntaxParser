:- module(tokenizer, 
    [tokenize/2]).
% the input(L) should be a list of ASCII characters. The string must be preprocessed.
% base
tokenize([], []) :- !. 
tokenize(L, [Word|Out]) :-
    L \== [], % list is not empty
    tokenize(L, Rest, WordChs), % identify words
    name(Word, WordChs), % convert the ASCII codes to a Prolog term
    tokenize(Rest, Out). % move onto rest of codes

tokenize([], [], []) :- !. % end of the word
tokenize([32|T], T, []) :- !. % '32 = space', end of word

tokenize([H|T], Rest, [H|List]) :-
    tokenize(T, Rest, List). % if not the end of a word, add code to output list and repeat

    
    