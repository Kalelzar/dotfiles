package(Pkg) :- xbps(Pkg); pacman(Pkg).

isXbps(Query) :- call(Query, 'void-repo-multilib').
isPacman(Query) :- call(Query, 'base').

isPackageManager(Query) :- isXbps(Query); isPacman(Query).

areEquivalent(A, A) :- xbps(A), pacman(A).
areEquivalent(A, B) :- equivalence(A, B).
areEquivalent(A, B) :- equivalence(B, A).
hasEquivalent(Pkg) :- areEquivalent(Pkg, _).

findEquivalent(In, Out) :- setof(Out, areEquivalent(In, Out), Set), member(Out, Set).

hasNoEquivalent(Pkg) :- \+(hasEquivalent(Pkg)).


% An empty list is a set.
set([], []).

% Put the head in the result,
% remove all occurrences of the head from the tail,
% make a set out of that.
set([H|T], [H|T1]) :-
    remv(H, T, T2),
    set(T2, T1).

% Removing anything from an empty list yields an empty list.
remv(_, [], []).

% If the head is the element we want to remove,
% do not keep the head and
% remove the element from the tail to get the new list.
remv(X, [X|T], T1) :- remv(X, T, T1).

% If the head is NOT the element we want to remove,
% keep the head and
% remove the element from the tail to get the new tail.
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).

equivalenceTreeWalk([], List, OutList) :- OutList = List.
equivalenceTreeWalk([Head | Tail], List, OutList) :-
    equivalenceTree(Head, List, EqList),
    equivalenceTreeWalk(Tail, EqList, OutList).


% E1. pacman, [], [pacman-git, xbps]
%  W2. [pacman-git | xbps], [pacman-git, xbps], []
%   E3. pacman-git, [pacman-git, xbps], [pacman-git, xbps, pacman]
%    W4. [pacman | Nil], [pacman-git, xbps, pacman], []

equivalenceTree(Pkg, List, FinalList) :-
    bagof(Out, areEquivalent(Pkg, Out), Set),
    append(List, Set, All),
    set(All, AllUnique),
    subtract(AllUnique, List, Diff),
    equivalenceTreeWalk(Diff, AllUnique, FinalList).

alternatives(Pkg, Alternative) :-
    equivalenceTree(Pkg, [], Alternatives),
    member(Alternative, Alternatives).


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
