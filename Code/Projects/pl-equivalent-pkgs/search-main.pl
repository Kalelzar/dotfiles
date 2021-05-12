main :-
    read(Input),
    validatePackage(Input),
    validateEquivalence(Input, Output),
    write(Output),nl,
    halt.

:- initialization(main).
