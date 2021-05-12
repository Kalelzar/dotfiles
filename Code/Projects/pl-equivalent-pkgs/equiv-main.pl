main :-
    read(Input),
    validatePackage(Input),
    forall(areEquivalent(Input, Output),
           (write(Output),nl)).

:- initialization(main).
