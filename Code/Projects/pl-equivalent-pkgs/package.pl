package(Pkg) :- xbps(Pkg); pacman(Pkg).

isXbps(Query) :- call(Query, 'xbps').
isPacman(Query) :- call(Query, 'pacman').

isPackageManager(Query) :- isXbps(Query); isPacman(Query).

areEquivalent(A, A) :- xbps(A), pacman(A).
areEquivalent(A, B) :- equivalence(A, B).
areEquivalent(A, B) :- equivalence(B, A).
hasEquivalent(Pkg) :- areEquivalent(Pkg, _).

findEquivalent(In, Out) :- setof(Out, areEquivalent(In, Out), Set), member(Out, Set).

hasNoEquivalent(Pkg) :- \+(hasEquivalent(Pkg)).

findNoEquivalent(PkgQuery, Pkg) :-
    isPackageManager(PkgQuery),
    call(PkgQuery, Pkg),
    hasNoEquivalent(Pkg).
findNoEquivalent(Pkg) :- findNoEquivalent(package, Pkg).

validatePackage(Pkg) :-
    package(Pkg) -> true;
    write(Pkg),
    write(' is not a package.'), nl,
    halt(1).
