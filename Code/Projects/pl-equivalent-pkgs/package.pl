both(Pkg) :- xbps(Pkg), pacman(Pkg).
package(Pkg) :- xbps(Pkg); pacman(Pkg).

isXbps(Query) :- call(Query, start__xbps).
isPacman(Query) :- call(Query, start__pacman).

isPackageManager(Query) :- isXbps(Query); isPacman(Query).

areEquivalent(A, B) :- equivalence(A, B); equivalence(B, A).
hasEquivalent(Pkg1) :- !, areEquivalent(Pkg1, _).

hasNoEquivalent(Pkg) :- hasEquivalent(Pkg) -> fail; true.

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
