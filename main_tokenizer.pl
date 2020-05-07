:- use_module(tokenizer).
:- use_module(preprocessing).

tokenizer(Token) :-
    writeln("Enter a sentence"),
    read(X), % read user's input
    preprocessing(X, L), % the input is a string and the output is a list of ASCII characters
    tokenize(L, Token), % pass the list of ASCII characters to tokenizer
    writeln(Token).

:- tokenizer(_).