:- use_module(tokenizer).
:- use_module(sr_parser).
:- use_module(preprocessing).

parser(Token, Tree) :-
    writeln("Enter a sentence"),
    read(X), % read user's input
    preprocessing(X, L), % the input is a string and the output is a list of ASCII characters
    tokenize(L, Token), % pass the list of ASCII characters to tokenizer
    writeln(Token),
    sr_parse([], Token, Tree), % parsing
    writeln(Tree).

:- parser(_, _).
