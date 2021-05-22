main :-
    read(Input),
    validatePackage(Input),
    forall(alternatives(Input, Output),
           (write(Output),nl)).

:- initialization(main).
