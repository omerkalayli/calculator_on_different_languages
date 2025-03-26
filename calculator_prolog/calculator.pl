calculator :-
    write('Prolog Calculator'), nl,
    loop.

loop :-
    write('> '),
    read_line_to_string(user_input, Input),
    ( Input = "quit" ; Input = "exit" -> 
        ( string_trim(Input, Trimmed),
          ( Trimmed = "" -> 
            write('Sonuç: 0'), nl, loop ;
            ( atom_string(AtomInput, Trimmed),
              atom_chars(AtomInput, Chars),
              phrase(expression(Result), Chars) -> 
                write('Sonuç: '), write(Result), nl, loop ;
                write('Error: Invalid!'), nl, loop
            )
          )
        )
    ).

expression(Result) --> term(T1), plus_or_minus(T1, Result).

plus_or_minus(Acc, Result) --> 
    [+], term(T), { NewAcc is Acc + T }, plus_or_minus(NewAcc, Result).
plus_or_minus(Acc, Result) --> 
    [-], term(T), { NewAcc is Acc - T }, plus_or_minus(NewAcc, Result).
plus_or_minus(Acc, Acc) --> [].

term(Result) --> factor(F1), mul_or_div(F1, Result).

mul_or_div(Acc, Result) --> 
    [*], factor(F), { NewAcc is Acc * F }, mul_or_div(NewAcc, Result).
mul_or_div(Acc, Result) --> 
    [/], factor(F), { F =:= 0 -> fail ; NewAcc is Acc / F }, mul_or_div(NewAcc, Result).
mul_or_div(Acc, Acc) --> [].

factor(Result) --> ['('], expression(Result), [')'].
factor(Result) --> number(Result).

number(Result) --> 
    digits(Digits), { number_chars(Result, Digits) }.

digits([D|T]) --> [D], { char_type(D, digit) }, digits(T).
digits([D])   --> [D], { char_type(D, digit) }.

string_trim(Input, Output) :-
    split_string(Input, " ", "", L),
    atomics_to_string(L, Output).