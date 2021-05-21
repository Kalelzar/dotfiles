main :-
    read(Input),
    validatePackage(Input),
    forall(findEquivalent(Input, Output),
           (write(Output),nl)).

:- initialization(main).
