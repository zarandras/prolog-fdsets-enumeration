
writecases([],_Vars,N,N) :- !, write('No cases.'), nl.
writecases(Ss,Vars,N,N1) :- writecases_(Ss,Vars,N,N1).

writecases_([],_Vars, N, N).
writecases_([S|Ss],Vars, N, N1) :-
    write('#'), write(N), write(':  '), 
    writecasebinary(S,Vars), 
%    write(' { '), writecase(S), write('} '), 
    nl, 
    N_ is N+1,
    writecases_(Ss,Vars,N_,N1).

%writecase([]).
%writecase([L|Ls]) :- writedeplist(L), writecase(Ls).
%
%writedeplist([]).
%writedeplist([D|Ds]) :- writedep(D), write(' '), writedeplist(Ds).
%
%writedep(d(L,A)) :- writeattrlist(L), write('->'), write(A).
%
%writeattrlist([]).
%writeattrlist([A|As]) :- write(A), writeattrlist(As).

% n=6:

writecasebinary([FL5,FL4,FL3,FL2,FL1], [A, B, C, D, E, F]) :-
    writebinary(FL5, A, B, C, D, E, F), write(' '),

    writebinary(FL4, A, B, C, D, E), write(' '),
    writebinary(FL4, A, B, C, D, F), write(' '),
    writebinary(FL4, A, B, C, E, F), write(' '),
    writebinary(FL4, A, B, D, E, F), write(' '),
    writebinary(FL4, A, C, D, E, F), write(' '),
    writebinary(FL4, B, C, D, E, F), write(' '),

    writebinary(FL3, A, B, C, D), write(' '),
    writebinary(FL3, A, B, C, E), write(' '),
    writebinary(FL3, A, B, C, F), write(' '),
    writebinary(FL3, A, B, D, E), write(' '),
    writebinary(FL3, A, B, D, F), write(' '),
    writebinary(FL3, A, B, E, F), write(' '),
    writebinary(FL3, A, C, D, E), write(' '),
    writebinary(FL3, A, C, D, F), write(' '),
    writebinary(FL3, A, C, E, F), write(' '),
    writebinary(FL3, A, D, E, F), write(' '),
    writebinary(FL3, B, C, D, E), write(' '),
    writebinary(FL3, B, C, D, F), write(' '),
    writebinary(FL3, B, C, E, F), write(' '),
    writebinary(FL3, B, D, E, F), write(' '),
    writebinary(FL3, C, D, E, F), write(' '),
    
    writebinary(FL2, A, B, C), write(' '),
    writebinary(FL2, A, B, D), write(' '),
    writebinary(FL2, A, B, E), write(' '),
    writebinary(FL2, A, B, F), write(' '),
    writebinary(FL2, A, C, D), write(' '),
    writebinary(FL2, A, C, E), write(' '),
    writebinary(FL2, A, C, F), write(' '),
    writebinary(FL2, A, D, E), write(' '),
    writebinary(FL2, A, D, F), write(' '),
    writebinary(FL2, A, E, F), write(' '),
    writebinary(FL2, B, C, D), write(' '),
    writebinary(FL2, B, C, E), write(' '),
    writebinary(FL2, B, C, F), write(' '),
    writebinary(FL2, B, D, E), write(' '),
    writebinary(FL2, B, D, F), write(' '),
    writebinary(FL2, B, E, F), write(' '),
    writebinary(FL2, C, D, E), write(' '),
    writebinary(FL2, C, D, F), write(' '),
    writebinary(FL2, C, E, F), write(' '),
    writebinary(FL2, D, E, F), write(' '),
    
    writebinary(FL1, A, B), write(' '),
    writebinary(FL1, A, C), write(' '),
    writebinary(FL1, A, D), write(' '),
    writebinary(FL1, A, E), write(' '),
    writebinary(FL1, A, F), write(' '),
    writebinary(FL1, B, C), write(' '),
    writebinary(FL1, B, D), write(' '),
    writebinary(FL1, B, E), write(' '),
    writebinary(FL1, B, F), write(' '),
    writebinary(FL1, C, D), write(' '),
    writebinary(FL1, C, E), write(' '),
    writebinary(FL1, C, F), write(' '),
    writebinary(FL1, D, E), write(' '),
    writebinary(FL1, D, F), write(' '),
    writebinary(FL1, E, F).

% n=5:

writecasebinary([FL4,FL3,FL2,FL1], [A, B, C, D, E]) :-
    writebinary(FL4, A, B, C, D, E), write(' '),

    writebinary(FL3, A, B, C, D), write(' '),
    writebinary(FL3, A, B, C, E), write(' '),
    writebinary(FL3, A, B, D, E), write(' '),
    writebinary(FL3, A, C, D, E), write(' '),
    writebinary(FL3, B, C, D, E), write(' '),
    
    writebinary(FL2, A, B, C), write(' '),
    writebinary(FL2, A, B, D), write(' '),
    writebinary(FL2, A, B, E), write(' '),
    writebinary(FL2, A, C, D), write(' '),
    writebinary(FL2, A, C, E), write(' '),
    writebinary(FL2, A, D, E), write(' '),
    writebinary(FL2, B, C, D), write(' '),
    writebinary(FL2, B, C, E), write(' '),
    writebinary(FL2, B, D, E), write(' '),
    writebinary(FL2, C, D, E), write(' '),
    
    writebinary(FL1, A, B), write(' '),
    writebinary(FL1, A, C), write(' '),
    writebinary(FL1, A, D), write(' '),
    writebinary(FL1, A, E), write(' '),
    writebinary(FL1, B, C), write(' '),
    writebinary(FL1, B, D), write(' '),
    writebinary(FL1, B, E), write(' '),
    writebinary(FL1, C, D), write(' '),
    writebinary(FL1, C, E), write(' '),
    writebinary(FL1, D, E).

% n=4:

writecasebinary([FL3,FL2,FL1], [A, B, C, D]) :-
    writebinary(FL3, A, B, C, D), write(' '),
    
    writebinary(FL2, A, B, C), write(' '),
    writebinary(FL2, A, B, D), write(' '),
    writebinary(FL2, A, C, D), write(' '),
    writebinary(FL2, B, C, D), write(' '),
    
    writebinary(FL1, A, B), write(' '),
    writebinary(FL1, A, C), write(' '),
    writebinary(FL1, A, D), write(' '),
    writebinary(FL1, B, C), write(' '),
    writebinary(FL1, B, D), write(' '),
    writebinary(FL1, C, D).

% n=3:

writecasebinary([FL2,FL1], [A, B, C]) :-
    writebinary(FL2, A, B, C), write(' '),
    
    writebinary(FL1, A, B), write(' '),
    writebinary(FL1, A, C), write(' '),
    writebinary(FL1, B, C).

% auxilliary:

writebinary(L, A, B, C, D, E, F) :-
    (memberdep(d([B, C, D, E, F], A), L) -> write(1); write(0)),
    (memberdep(d([A, C, D, E, F], B), L) -> write(1); write(0)),
    (memberdep(d([A, B, D, E, F], C), L) -> write(1); write(0)),
    (memberdep(d([A, B, C, E, F], D), L) -> write(1); write(0)),
    (memberdep(d([A, B, C, D, F], E), L) -> write(1); write(0)),
    (memberdep(d([A, B, C, D, E], F), L) -> write(1); write(0)).

writebinary(L, A, B, C, D, E) :-
    (memberdep(d([B, C, D, E], A), L) -> write(1); write(0)),
    (memberdep(d([A, C, D, E], B), L) -> write(1); write(0)),
    (memberdep(d([A, B, D, E], C), L) -> write(1); write(0)),
    (memberdep(d([A, B, C, E], D), L) -> write(1); write(0)),
    (memberdep(d([A, B, C, D], E), L) -> write(1); write(0)).

writebinary(L, A, B, C, D) :-
    (memberdep(d([B, C, D], A), L) -> write(1); write(0)),
    (memberdep(d([A, C, D], B), L) -> write(1); write(0)),
    (memberdep(d([A, B, D], C), L) -> write(1); write(0)),
    (memberdep(d([A, B, C], D), L) -> write(1); write(0)).

writebinary(L, A, B, C) :-
    (memberdep(d([B, C], A), L) -> write(1); write(0)),
    (memberdep(d([A, C], B), L) -> write(1); write(0)),
    (memberdep(d([A, B], C), L) -> write(1); write(0)).

writebinary(L, A, B) :-
    (memberdep(d([B], A), L) -> write(1); write(0)),
    (memberdep(d([A], B), L) -> write(1); write(0)).

writebinary(L, A) :-
    (memberdep(d([], A), L) -> write(1); write(0)).



% classificative presentation

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

dimcomb3([RA, RB, RC]) :- dim3(RA), dim3(RB), RA=<RB, dim3(RC), RB=<RC.
dimcomb4([RA, RB, RC, RD]) :- dim4(RA), dim4(RB), RA=<RB, dim4(RC), RB=<RC, dim4(RD), RC=<RD.
dimcomb5([RA, RB, RC, RD, RE]) :- dim5(RA), dim5(RB), RA=<RB, dim5(RC), RB=<RC, dim5(RD), RC=<RD, dim5(RE), RD=<RE.
dimcomb6([RA, RB, RC, RD, RE, RF]) :- dim6(RA), dim6(RB), RA=<RB, dim6(RC), RB=<RC, dim6(RD), RC=<RD, dim6(RE), RD=<RE, 
    dim6(RF), RE=<RF.

permnr(L,N) :- permnr_(L,Fprod,Pprod,_,_), N is Fprod//Pprod.
permnr_([_I],1,1,2,2).
permnr_([I,I|Is],Fprod,Pprod,Fnext,Pnext) :- !, 
    permnr_([I|Is],Fp,Pp,Fn,Pn), Fprod is Fp*Fn, Pprod is Pp*Pn, Fnext is Fn+1, Pnext is Pn+1.
permnr_([_I,J|Js],Fprod,Pprod,Fnext,Pnext) :- !,
    permnr_([J|Js],Fp,Pp,Fn,_Pn), Fprod is Fp*Fn, Pprod is Pp, Fnext is Fn+1, Pnext=2.

presentallclasses(A, B, C) :-
    write('n=3'), nl, nl,
    findall(R,dimcomb3(R),Rs), presentclasses([A, B, C], Rs, 0, 0, N, RN),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CASES: '), write(N), write('; TOTAL DIFFERENT CASES: '), write(RN), nl, nl, ttyflush.

presentallclasses(A, B, C, D) :-
    write('n=4'), nl, nl,
    findall(R,dimcomb4(R),Rs), presentclasses([A, B, C, D], Rs, 0, 0, N, RN),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CASES: '), write(N), write('; TOTAL DIFFERENT CASES: '), write(RN), nl, nl, ttyflush.

presentallclasses(A, B, C, D, E) :-
    write('n=5'), nl, nl,
    findall(R,dimcomb5(R),Rs), presentclasses([A, B, C, D, E], Rs, 0, 0, N, RN),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CASES: '), write(N), write('; TOTAL DIFFERENT CASES: '), write(RN), nl, nl, ttyflush.

presentallclasses(A, B, C, D, E, F) :-
    write('n=6'), nl, nl,
    findall(R,dimcomb6(R),Rs), presentclasses([A, B, C, D, E, F], Rs, 0, 0, N, RN),
    write('----------------------------------'), nl,
    write('----------------------------------'), nl,
    write('TOTAL CASES: '), write(N), write('; TOTAL DIFFERENT CASES: '), write(RN), nl, nl, ttyflush.

writeheaderitem(A, RA) :- write('['), write(A), write(']='), write(RA).

writeclassheader([A],[RA]) :- !, writeheaderitem(A, RA), nl.
writeclassheader([A|As],[RA|RAs]) :- writeheaderitem(A, RA), write(', '), writeclassheader(As,RAs).


presentclasses(_Vars, [], N, RN, N, RN).
presentclasses(Vars, [C|Cs], InN, InRN, OutN, OutRN) :-
    writeclassheader(Vars, C), 
    write('----------------------------------'), nl,
    presentclass(Vars, C, InN, InRN, N1, RN1),
    write('----------------------------------'), nl,
    nl, ttyflush,
    presentclasses(Vars, Cs, N1, RN1, OutN, OutRN).

presentclass(Vars, Class, InN, InRN, OutN, OutRN) :-
    write('All cases: '), nl,
    casesf(Ss_,N,Vars,Class), 
    writecases(Ss_,Vars,InN,_), permnr(Class,PN), OutN is InN+N*PN,
    write(PN), write(' * '), write(N), write(' case(s).'), nl, nl, ttyflush,
    write('Different cases: '), nl, 
    reducecasesf(Ss_,Ss,Vars),
    writecases(Ss,Vars,InRN,OutRN),
    length(Ss,RN), write(RN), write(' case(s).'), nl, nl, ttyflush.

casesf(Ss,N,[A,B,C],[RA,RB,RC]) :-
    cases3f(Ss,N,A,B,C,RA,RB,RC).

casesf(Ss,N,[A,B,C,D],[RA,RB,RC,RD]) :-
    cases4f(Ss,N,A,B,C,D,RA,RB,RC,RD).

casesf(Ss,N,[A,B,C,D,E],[RA,RB,RC,RD,RE]) :-
    cases5f(Ss,N,A,B,C,D,E,RA,RB,RC,RD,RE).

casesf(Ss,N,[A,B,C,D,E,F],[RA,RB,RC,RD,RE,RF]) :-
    cases6f(Ss,N,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF).


reducecasesf(Ss,RSs,[A,B,C]) :-
    reducecases3f(Ss,RSs,A,B,C).

reducecasesf(Ss,RSs,[A,B,C,D]) :-
    reducecases4f(Ss,RSs,A,B,C,D).

reducecasesf(Ss,RSs,[A,B,C,D,E]) :-
    reducecases5f(Ss,RSs,A,B,C,D,E).

reducecasesf(Ss,RSs,[A,B,C,D,E,F]) :-
    reducecases6f(Ss,RSs,A,B,C,D,E,F).
