
member(X,[X | _]).
member(X,[_ | Ys]) :- member(X,Ys).

memberdep(d([X],A),[d([X],A) | _]).
memberdep(d([X,Y],A),[d([U,V],A) | _]) :- u([U,V],X,Y).
memberdep(d([X,Y,Z],A),[d([U,V,W],A) | _]) :- u([U,V,W],X,Y,Z).
memberdep(D,[_ | Ls]) :- memberdep(D,Ls).

memberdepexcl(d([X],A),[d([X],A) | Ls],Ls) :- !.
memberdepexcl(d([X,Y],A),[d([U,V],A) | Ls],Ls) :- u([U,V],X,Y), !.
memberdepexcl(d([X,Y,Z],A),[d([U,V,W],A) | Ls],Ls) :- u([U,V,W],X,Y,Z), !.
memberdepexcl(D,[L1 | Ls],[L1 | LsE]) :- memberdepexcl(D,Ls,LsE).

similardeps([],[]).
similardeps([D | L1s],L2) :- memberdepexcl(D,L2,L2s), similardeps(L1s,L2s).

similarcasef([],[]).
similarcasef([L | Ls],[L2 | L2s]) :- similardeps(L,L2), similarcasef(Ls,L2s).


append([],Ys,Ys).
append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).

append(Ws, Xs, Ys, Zs) :- append(Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Vs, Ws, Xs, Ys, Zs) :- append(Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).

min(X,Y,M) :- X>Y, !, M=Y.
min(X,_,X).

min(X,Y,Z,M) :- min(X,Y,M1), min(M1,Z,M).


u([X, Y],X,Y).
u([Y, X],X,Y).

u([X | YZ],X,Y,Z) :- u(YZ,Y,Z).
u([Y | XZ],X,Y,Z) :- u(XZ,X,Z).
u([Z | XY],X,Y,Z) :- u(XY,X,Y).

u([W | XYZ],W,X,Y,Z) :- u(XYZ,X,Y,Z).
u([X | WYZ],W,X,Y,Z) :- u(WYZ,W,Y,Z).
u([Y | WXZ],W,X,Y,Z) :- u(WXZ,W,X,Z).
u([Z | WXY],W,X,Y,Z) :- u(WXY,W,X,Y).


replace([],[],_,_).
replace([[] | Ss],[[] | Ds],SC,DC) :- !, 
    replace(Ss,Ds,SC,DC).
replace([S | Ss],[D | Ds],SC,DC) :- atom(S), !, 
    repl_atom(S,D,SC,DC), 
    replace(Ss,Ds,SC,DC).
replace([S | Ss],[D | Ds],SC,DC) :- S=d(US,AS), !, 
    D=d(UD,AD), replace(US,UD,SC,DC), repl_atom(AS,AD,SC,DC), 
    replace(Ss,Ds,SC,DC).
replace([S | Ss],[D | Ds],SC,DC) :- S=[_ | _], !,
    replace(S,D,SC,DC),
    replace(Ss,Ds,SC,DC).

repl_atom(S,S,[],[]) :- !.
repl_atom(S,D,[S | _],[D | _]) :- !.
repl_atom(S,D,[_ | SCs],[_ | DCs]) :- !, repl_atom(S,D,SCs,DCs).


reducecasesf([],[]).
reducecasesf([S | Ss],RSs) :- 
    member(S2,Ss), similarcasef(S,S2), !,
    reducecasesf(Ss,RSs).
reducecasesf([S | Ss],[S | RSs]) :- reducecasesf(Ss,RSs).


case2([],_,_,100,100).
case2([d([A],B)],A,B,100,1).
case2([d([B],A)],A,B,1,100).
case2([d([A],B), d([B],A)],A,B,1,1).

case2f([FL1],A,B,RA,RB) :- case2(FL1,A,B,RA,RB).

cases2f(Ss,N,A,B,RA,RB) :- 
    findall(S,case2f(S,A,B,RA,RB),Ss),length(Ss,N).

reducecases2f([],[],_,_).
reducecases2f([S | Ss],RSs,X,Y) :- 
    u(PermXY,X,Y), 
    replace(S,SP,[X,Y],PermXY), 
    member(S2,Ss), similarcasef(SP,S2), !,
    reducecases2f(Ss,RSs,X,Y).
reducecases2f([S | Ss],[S | RSs],X,Y) :- reducecases2f(Ss,RSs,X,Y).

cases2fr(RSs,RN,A,B,RA,RB) :-
    cases2f(Ss,_,A,B,RA,RB), reducecases2f(Ss,RSs,A,B), length(RSs,RN).

cases2ff(RSs,RN,A,B,RA,RB) :-
    cases2f(Ss,_,A,B,RA,RB), reducecasesf(Ss,RSs), length(RSs,RN).


case3(LABC,LAB,LAC,LBC,A,B,C,RA,RB,RC) :- 

    case2(LAB,A,B,RA1,RB1), case2(LAC,A,C,RA2,RC1), case2(LBC,B,C,RB2,RC2),

    min(RA1,RA2,RAm), min(RB1,RB2,RBm), min(RC1,RC2,RCm),
    
    (RAm<100 -> (LABC=[d([B,C],A) | LABC1], RA=RAm)
     ; ((LABC=LABC1, RA=100); (LABC=[d([B,C],A) | LABC1], RA=2))),
    (RBm<100 -> (LABC1=[d([A,C],B) | LABC2], RB=RBm)
     ; ((LABC1=LABC2, RB=100); (LABC1=[d([A,C],B) | LABC2], RB=2))),
    (RCm<100 -> (LABC2=[d([A,B],C)], RC=RCm)
     ; ((LABC2=[], RC=100); (LABC2=[d([A,B],C)], RC=2))),
    
    \+t1(LABC,LAB,LAC,LBC).

t1(LABC,LAB,LAC,LBC) :- append(LAB,LAC,LBC,LABACBC),
    memberdep(d([X,Y],Z),LABC), 
    memberdep(d([X],Y),LABACBC),
    \+memberdep(d([X],Z),LABACBC).

case3f([FL2,FL1],A,B,C,RA,RB,RC) :-
    case3(FL2,LAB,LAC,LBC,A,B,C,RA,RB,RC),
    append(LAB,LAC,LBC,FL1).

cases3f(Ss,N,A,B,C,RA,RB,RC) :- 
    findall(S,case3f(S,A,B,C,RA,RB,RC),Ss),length(Ss,N).

reducecases3f([],[],_,_,_).
reducecases3f([S | Ss],RSs,X,Y,Z) :- 
    u(PermXYZ,X,Y,Z), replace(S,SP,[X,Y,Z],PermXYZ), 
    member(S2,Ss), similarcasef(SP,S2), !,
    reducecases3f(Ss,RSs,X,Y,Z).
reducecases3f([S | Ss],[S | RSs],X,Y,Z) :- reducecases3f(Ss,RSs,X,Y,Z).

cases3fr(RSs,RN,A,B,C,RA,RB,RC) :-
    cases3f(Ss,_,A,B,C,RA,RB,RC), reducecases3f(Ss,RSs,A,B,C), length(RSs,RN).

cases3ff(RSs,RN,A,B,C,RA,RB,RC) :-
    cases3f(Ss,_,A,B,C,RA,RB,RC), reducecasesf(Ss,RSs), length(RSs,RN).


case4(LABCD,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,A,B,C,D,RA,RB,RC,RD) :- 
    case3(LABC,LAB,LAC,LBC,A,B,C,RA1,RB1,RC1), 
    case3(LABD,LAB,LAD,LBD,A,B,D,RA2,RB2,RD1),
    case3(LACD,LAC,LAD,LCD,A,C,D,RA3,RC2,RD2), 
    case3(LBCD,LBC,LBD,LCD,B,C,D,RB3,RC3,RD3),

    min(RA1,RA2,RA3,RAm), min(RB1,RB2,RB3,RBm),
    min(RC1,RC2,RC3,RCm), min(RD1,RD2,RD3,RDm),

    (RAm<100 -> (LABCD=[d([B,C,D],A) | LABCD1], RA=RAm)
     ; ((LABCD=LABCD1, RA=100); (LABCD=[d([B,C,D],A) | LABCD1], RA=3))),
    (RBm<100 -> (LABCD1=[d([A,C,D],B) | LABCD2], RB=RBm)
     ; ((LABCD1=LABCD2, RB=100); (LABCD1=[d([A,C,D],B) | LABCD2], RB=3))),
    (RCm<100 -> (LABCD2=[d([A,B,D],C) | LABCD3], RC=RCm)
     ; ((LABCD2=LABCD3, RC=100); (LABCD2=[d([A,B,D],C) | LABCD3], RC=3))),
    (RDm<100 -> (LABCD3=[d([A,B,C],D)], RD=RDm)
     ; ((LABCD3=[], RD=100); (LABCD3=[d([A,B,C],D)], RD=3))),

    \+t2(LABCD,LABC,LABD,LACD,LBCD).

t2(LABCD,LABC,LABD,LACD,LBCD) :- 
    append(LABC,LABD,LACD,LBCD,LABCABDACDBCD), 
    memberdep(d([X,Y,Z],W),LABCD),
    memberdep(d([X,Y],Z),LABCABDACDBCD),
    \+memberdep(d([X,Y],W),LABCABDACDBCD).

case4f([FL3,FL2,FL1],A,B,C,D,RA,RB,RC,RD) :-
    case4(FL3,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,A,B,C,D,RA,RB,RC,RD),
    append(LABC,LABD,LACD,LBCD,FL2), append(LAB,LAC,LAD,LBC,LBD,LCD,FL1).

cases4f(Ss,N,A,B,C,D,RA,RB,RC,RD) :- 
    findall(S,case4f(S,A,B,C,D,RA,RB,RC,RD),Ss),length(Ss,N).

reducecases4f([],[],_,_,_,_).
reducecases4f([S | Ss],RSs,W,X,Y,Z) :- 
    u(PermWXYZ,W,X,Y,Z), 
    replace(S,SP,[W,X,Y,Z],PermWXYZ), 
    member(S2,Ss), similarcasef(SP,S2), !,
    reducecases4f(Ss,RSs,W,X,Y,Z).
reducecases4f([S | Ss],[S | RSs],W,X,Y,Z) :- reducecases4f(Ss,RSs,W,X,Y,Z).

cases4fr(RSs,RN,A,B,C,D,RA,RB,RC,RD) :-
    cases4f(Ss,_,A,B,C,D,RA,RB,RC,RD), reducecases4f(Ss,RSs,A,B,C,D), 
    length(RSs,RN).

cases4ff(RSs,RN,A,B,C,D,RA,RB,RC,RD) :-
    cases4f(Ss,_,A,B,C,D,RA,RB,RC,RD), reducecasesf(Ss,RSs), 
    length(RSs,RN).


case4fr(RFL3,RFL2,RFL1,A,B,C,D,RA,RB,RC,RD) :- 
    cases4fr(RSs,_,A,B,C,D,RA,RB,RC,RD), 
    member([RFL3,RFL2,RFL1],RSs).
