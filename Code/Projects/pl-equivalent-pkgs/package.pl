both(T) :- xbps(T), pacman(T).
package(T) :- xbps(T); pacman(T).

hasEquivalent(A) :- equivalence(A, B); equivalence(B, A).
areEquivalent(A, B) :- equivalence(A, B); equivalence(B, A).

validatePackage(Input) :-
    package(Input),
    !, true;
    write(Input),
    write(' is not a package.'), nl,
    halt(1).

validateEquivalence(Input, Output) :-
    areEquivalent(Input, Output),
    !, true;
    (xbps(Input), !, write('XBPS '); pacman(Input), write('Pacman ')),
    write('package '),
    write(Input),
    write(' has no equivalent'), nl,
    halt(2).

main :-
    read(Input),
    validatePackage(Input),
    validateEquivalence(Input, Output),
    write(Output),nl,
    halt.

:- initialization(main).
