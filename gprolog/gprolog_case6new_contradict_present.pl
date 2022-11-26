
writecasesfc([],_Vars,N,N1) :- !, N1 is N+1, write('Contradictory class.'), nl.
writecasesfc(_,_Vars,N,N) :- write('Non-contradictory class.'), nl.

dim1(100).
dim2(RA) :- dim1(RA).
dim2(1).
dim3(RA) :- dim2(RA).
dim3(2).
dim4(RA) :- dim3(RA).
dim4(3).
dim5(RA) :- dim4(RA).
dim5(4).
dim6(RA) :- dim5(RA).
dim6(5).

dimcomb2([RA, RB]) :- dim2(RA), dim2(RB), RA=<RB.
dimcomb3([RA, RB, RC]) :- dim3(RA), dim3(RB), RA=<RB, dim3(RC), RB=<RC.
dimcomb4([RA, RB, RC, RD]) :- dim4(RA), dim4(RB), RA=<RB, dim4(RC), RB=<RC, dim4(RD), RC=<RD.
dimcomb5([RA, RB, RC, RD, RE]) :- dim5(RA), dim5(RB), RA=<RB, dim5(RC), RB=<RC, dim5(RD), RC=<RD, dim5(RE), RD=<RE.
dimcomb6([RA, RB, RC, RD, RE, RF]) :- dim6(RA), dim6(RB), RA=<RB, dim6(RC), RB=<RC, dim6(RD), RC=<RD, dim6(RE), RD=<RE, 
    dim6(RF), RE=<RF.

presentallclasses(A, B) :-
    A@<B,
    write('n=2'), nl, nl,
    findall(R,dimcomb2(R),Rs), presentclasses(Rs, [A, B], 0, N),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CONTRADICTORY CLASSES: '), write(N), nl, nl.

presentallclasses(A, B, C) :-
    A@<B, B@<C,
    write('n=3'), nl, nl,
    findall(R,dimcomb3(R),Rs), presentclasses(Rs, [A, B, C], 0, N),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CONTRADICTORY CLASSES: '), write(N), nl, nl.

presentallclasses(A, B, C, D) :-
    A@<B, B@<C, C@<D,
    write('n=4'), nl, nl,
    findall(R,dimcomb4(R),Rs), presentclasses(Rs, [A, B, C, D], 0, N),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CONTRADICTORY CLASSES: '), write(N), nl, nl.

presentallclasses(A, B, C, D, E) :-
    A@<B, B@<C, C@<D, D@<E,
    write('n=5'), nl, nl,
    findall(R,dimcomb5(R),Rs), presentclasses(Rs, [A, B, C, D, E], 0, N),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CONTRADICTORY CLASSES: '), write(N), nl, nl.

presentallclasses(A, B, C, D, E, F) :-
    A@<B, B@<C, C@<D, D@<E, E@<F,
    write('n=6'), nl, nl,
    findall(R,dimcomb6(R),Rs), presentclasses(Rs, [A, B, C, D, E, F], 0, N),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CONTRADICTORY CLASSES: '), write(N), nl, nl.

writeheaderitem(A, RA) :- write('['), write(A), write(']='), write(RA).

writeclassheader([A],[RA]) :- !, writeheaderitem(A, RA), nl.
writeclassheader([A|As],[RA|RAs]) :- writeheaderitem(A, RA), write(', '), writeclassheader(As,RAs).


presentclasses([], _Vars, N, N).
presentclasses([C|Cs], Vars, InN, OutN) :-
    writeclassheader(Vars, C), 
    write('----------------------------------'), nl,
    presentclass(C, Vars, InN, N1),
    write('----------------------------------'), nl,
    nl, 
    presentclasses(Cs, Vars, N1, OutN).

presentclass(Class, Vars, InN, OutN) :-
    casesfc(Ss,Vars,Class), 
    writecasesfc(Ss,Vars,InN,OutN),
    nl, nl.

casesfc(Ss,[A,B],[RA,RB]) :-
    case2fc(Cs,A,B,RA,RB), !, Ss=[Cs].

casesfc(Ss,[A,B,C],[RA,RB,RC]) :-
    case3fc(Cs,A,B,C,RA,RB,RC), !, Ss=[Cs].

casesfc(Ss,[A,B,C,D],[RA,RB,RC,RD]) :-
    case4fc(Cs,A,B,C,D,RA,RB,RC,RD), !, Ss=[Cs].

casesfc(Ss,[A,B,C,D,E],[RA,RB,RC,RD,RE]) :-
    case5fc(Cs,A,B,C,D,E,RA,RB,RC,RD,RE), !, Ss=[Cs].

casesfc(Ss,[A,B,C,D,E,F],[RA,RB,RC,RD,RE,RF]) :-
    case6fc(Cs,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF), !, Ss=[Cs].

casesfc([],_,_).
