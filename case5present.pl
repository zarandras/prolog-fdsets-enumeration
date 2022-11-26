
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

dimcomb3([RA, RB, RC]) :- dim3(RA), dim3(RB), RA=<RB, dim3(RC), RB=<RC.
dimcomb4([RA, RB, RC, RD]) :- dim4(RA), dim4(RB), RA=<RB, dim4(RC), RB=<RC, dim4(RD), RC=<RD.
dimcomb5([RA, RB, RC, RD, RE]) :- dim5(RA), dim5(RB), RA=<RB, dim5(RC), RB=<RC, dim5(RD), RC=<RD, dim5(RE), RD=<RE.

presentallclasses(A, B, C) :-
    findall(R,dimcomb3(R),Rs), presentclasses([A, B, C], Rs, 0).

presentallclasses(A, B, C, D) :-
    findall(R,dimcomb4(R),Rs), presentclasses([A, B, C, D], Rs, 0).

presentallclasses(A, B, C, D, E) :-
    findall(R,dimcomb5(R),Rs), presentclasses([A, B, C, D, E], Rs, 0).


writeheaderitem(A, RA) :- write('['), write(A), write(']='), write(RA).

writeclassheader([A],[RA]) :- !, writeheaderitem(A, RA), nl.
writeclassheader([A|As],[RA|RAs]) :- writeheaderitem(A, RA), write(', '), writeclassheader(As,RAs).


presentclasses(_Vars, [], _).
presentclasses(Vars, [C|Cs], N) :-
    writeclassheader(Vars, C), 
    write('----------------------------------'), nl,
    casesbases(Ss, SBs, Vars, C), writecases(Ss, SBs, Vars, N, N1),
    write('----------------------------------'), nl,
    nl, ttyflush,
    presentclasses(Vars, Cs, N1).

casesbases(Ss, SBs,[A, B, C],[RA, RB, RC]) :-
%    cases3fr(Ss,_,A,B,C,RA,RB,RC), 
    write('All cases: '),nl,
    cases3f(Ss_,N,A,B,C,RA,RB,RC), 
    write(Ss_),nl,write(N), write(' case(s).'), nl, ttyflush,
    reducecases3f(Ss_,Ss,A,B,C),
    write('Different cases: '),nl,write(Ss),nl,
    length(Ss,RN), write(RN), write(' case(s).'), nl, ttyflush,
    write('Formatted (Binary & Bases): '),nl,
    bases(Ss,SBs).

casesbases(Ss, SBs,[A, B, C, D],[RA, RB, RC, RD]) :-
%    cases4fr(Ss,_,A,B,C,D,RA,RB,RC,RD), 
    write('All cases: '),nl,
    cases4f(Ss_,N,A,B,C,D,RA,RB,RC,RD), 
    write(Ss_),nl,write(N), write(' case(s).'), nl, ttyflush,
    reducecases4f(Ss_,Ss,A,B,C,D),
    write('Different cases: '),nl,write(Ss),nl,
    length(Ss,RN), write(RN), write(' case(s).'), nl, ttyflush,
    write('Formatted (Binary & Bases): '),nl,
    bases(Ss,SBs).

casesbases(Ss, SBs,[A, B, C, D, E],[RA, RB, RC, RD, RE]) :-
%    cases5fr(Ss,_,A,B,C,D,E,RA,RB,RC,RD, RE), 
    write('All cases: '),nl,
    cases5f(Ss_,N,A,B,C,D,E,RA,RB,RC,RD,RE), 
    write(Ss_),nl,write(N), write(' case(s).'), nl, ttyflush,
    reducecases5f(Ss_,Ss,A,B,C,D,E),
    write('Different cases: '),nl,write(Ss),nl,
    length(Ss,RN), write(RN), write(' case(s).'), nl, ttyflush,
    write('Formatted (Binary & Bases): '),nl,
    bases(Ss,SBs).



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
