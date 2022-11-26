
writecases([],[],_Vars,N,N) :- !, write('No cases.'), nl.
writecases(Ss,SBs,Vars,N,N1) :- writecases_(Ss,SBs,Vars,N,N1), nl.

writecases_([],[],_Vars, N, N).
writecases_([S|Ss],[SB|SBs],Vars, N, N1) :-
    write('#'), write(N), write(':  '), 
    writecasebinary(S,Vars), 
    write(' { '), writecase(SB), write('} '), nl, 
    N_ is N+1,
    writecases_(Ss,SBs,Vars,N_,N1).

writecase([]).
writecase([L|Ls]) :- writedeplist(L), writecase(Ls).

writedeplist([]).
writedeplist([D|Ds]) :- writedep(D), write(' '), writedeplist(Ds).

writedep(d(L,A)) :- writeattrlist(L), write('->'), write(A).

writeattrlist([]).
writeattrlist([A|As]) :- write(A), writeattrlist(As).

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

dimcomb3([RA, RB, RC]) :- dim3(RA), dim3(RB), RA=<RB, dim3(RC), RB=<RC.
dimcomb4([RA, RB, RC, RD]) :- dim4(RA), dim4(RB), RA=<RB, dim4(RC), RB=<RC, dim4(RD), RC=<RD.

presentallclasses(A, B, C) :-
    findall(R,dimcomb3(R),Rs), presentclasses([A, B, C], Rs, 0).

presentallclasses(A, B, C, D) :-
    findall(R,dimcomb4(R),Rs), presentclasses([A, B, C, D], Rs, 0).


writeheaderitem(A, RA) :- write('['), write(A), write(']='), write(RA).

writeclassheader([A],[RA]) :- !, writeheaderitem(A, RA), nl.
writeclassheader([A|As],[RA|RAs]) :- writeheaderitem(A, RA), write(', '), writeclassheader(As,RAs).


presentclasses(_Vars, [], _).
presentclasses(Vars, [C|Cs], N) :-
    writeclassheader(Vars, C), 
    write('----------------------------------'), nl,
    casesbases(Ss, SBs, Vars, C), writecases(Ss, SBs, Vars, N, N1),
    write('----------------------------------'), nl,
    nl,
    presentclasses(Vars, Cs, N1).

casesbases(Ss, SBs,[A, B, C],[RA, RB, RC]) :-
    cases3fr(Ss,_,A,B,C,RA,RB,RC), bases(Ss,SBs).

casesbases(Ss, SBs,[A, B, C, D],[RA, RB, RC, RD]) :-
    cases4fr(Ss,_,A,B,C,D,RA,RB,RC,RD), bases(Ss,SBs).


% bases: case reduction

bases([],[]).
bases([S|Ss],[SB|SBs]) :- base(S,SB), bases(Ss,SBs).

base([FL],[FLB]) :- 
    filtertrans(FL,FLB).

base([FLH,FLL|FLs],[FLHB,FLLB|FLBs]) :- 
    simplifyT(FLH,FLL,FLH1,FLL1),
    filtertrans(FLH1,FLH1_),
    equivT(FLH1_,FLL1,FLH2,FLL2), 
    simplifyS(FLH2,FLL2,FLHB,FLL3),
    base([FLL3|FLs],[FLLB|FLBs]).

simplifyT(FLH,FLL,FLHB,FLLB) :- 
    memberdep(d(L,A),FLL), memberdep(d([A|L],B),FLH), memberdepexcl(d(L,B),FLL,FLL2), 
    !, 
    simplifyT(FLH,FLL2,FLHB,FLLB).
simplifyT(FLH,FLL,FLH,FLL).

simplifyS(FLH,FLL,FLHB,FLLB) :- 
    memberdep(d(L,A),FLL), memberdepexcl(d([_|L],A),FLH,FLH2),
    !,
    simplifyS(FLH2,FLL,FLHB,FLLB).
simplifyS(FLH,FLL,FLH,FLL).

equivT(FLH,FLL,FLHB,FLLB) :- 
    memberdep(d(L,A),FLL), memberdepexcl(d([A|L],B),FLH,FLH2),
    !, (memberdep(d(L,B),FLL) -> FLL2=FLL; FLL2=[d(L,B)|FLL]),
    equivT(FLH2,FLL2,FLHB,FLLB).
equivT(FLH,FLL,FLH,FLL).

filtertrans(FL,FLB) :-
    memberdep(d([C|L],A),FL), memberdep(d([A|L],B),FL), memberdepexcl(d([C|L],B),FL,FL1),
    !,
    filtertrans(FL1,FLB).
filtertrans(FL,FL).
