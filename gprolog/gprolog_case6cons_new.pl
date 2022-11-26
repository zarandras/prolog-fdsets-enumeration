
member(X,[X | _]).
member(X,[_ | Ys]) :- member(X,Ys).

%memberdep(d([X],A),[d([X],A) | _]).
%memberdep(d([X,Y],A),[d([U,V],A) | _]) :- u([U,V],X,Y).
%memberdep(d([X,Y,Z],A),[d([U,V,W],A) | _]) :- u([U,V,W],X,Y,Z).
%memberdep(d([W,X,Y,Z],A),[d([S,T,U,V],A) | _]) :- u([S,T,U,V],W,X,Y,Z).
%memberdep(d([V1,W,X,Y,Z],A),[d([R,S,T,U,V],A) | _]) :- u([R,S,T,U,V],V1,W,X,Y,Z).
%memberdep(D,[_ | Ls]) :- memberdep(D,Ls).

%memberdepexcl(d([X],A),[d([X],A) | Ls],Ls) :- !.
%memberdepexcl(d([X,Y],A),[d([U,V],A) | Ls],Ls) :- u([U,V],X,Y), !.
%memberdepexcl(d([X,Y,Z],A),[d([U,V,W],A) | Ls],Ls) :- u([U,V,W],X,Y,Z), !.
%memberdepexcl(d([W,X,Y,Z],A),[d([S,T,U,V],A) | Ls],Ls) :- 
%    u([S,T,U,V],W,X,Y,Z), !.
%memberdepexcl(d([V1,W,X,Y,Z],A),[d([R,S,T,U,V],A) | Ls],Ls) :- 
%    u([R,S,T,U,V],V1,W,X,Y,Z), !.
%memberdepexcl(D,[L1 | Ls],[L1 | LsE]) :- memberdepexcl(D,Ls,LsE).

%similardeps([],[]).
%similardeps([D | L1s],L2) :- memberdepexcl(D,L2,L2s), similardeps(L1s,L2s).

%similarcasef([],[]).
%similarcasef([L | Ls],[L2 | L2s]) :- length(L,LN), length(L2,LN), similardeps(L,L2), similarcasef(Ls,L2s).

similarcasef([],[],_,_).
similarcasef([L | Ls],[L2 | L2s],SC,DC) :- length(L,LN), length(L2,LN), 
    replacel(L2,L2P,SC,DC), sort(L2P,L), 
    similarcasef(Ls,L2s,SC,DC).

firstcasec([],[]).
firstcasec([[Crec|_] | _],Crec).

similarcasefcfound([[Crec|Ls1] | _],Crec,Ls,SC,DC) :-
    similarcasef(Ls1,Ls,SC,DC), !.

similarcasefcfound([[Crec|_] | Ss],Crec,Ls,SC,DC) :-
    similarcasefcfound(Ss,Crec,Ls,SC,DC).


merge0([],L,L).
merge0([X|Xs],L,Ms) :- merge0_(L,X,Xs,Ms).

merge0_([],Y,Ys,[Y|Ys]).
merge0_([X|Xs],Y,Ys,Ms) :- X@<Y, !, Ms=[X|Ms2], merge0_(Xs,Y,Ys,Ms2).
merge0_([X|Xs],Y,Ys,[Y|Ms2]) :- merge0_(Ys,X,Xs,Ms2).


merge([],L,L,Len) :- length(L,Len).
merge([X|Xs],L,Ms,Len) :- merge_(L,X,Xs,Ms,Len).

merge_([],Y,Ys,[Y|Ys],Len) :- length([Y|Ys],Len).
merge_([X|Xs],Y,Ys,Ms,Len) :- X@<Y, !, Ms=[X|Ms2], merge_(Xs,Y,Ys,Ms2,Len1), Len is Len1+1.
merge_([X|Xs],Y,Ys,[Y|Ms2],Len) :- merge_(Ys,X,Xs,Ms2,Len1), Len is Len1+1.

merge([],Xs,Ys,Ms,Len) :- !, merge(Xs,Ys,Ms,Len).
merge(Xs,[],Ys,Ms,Len) :- !, merge(Xs,Ys,Ms,Len).
merge([X|Xs],[Y|Ys],Zs,Ms,Len) :- merge_(Zs,X,Xs,Y,Ys,Ms,Len).

merge_([],Y,Ys,Z,Zs,Ms,Len) :- merge_([Y|Ys],Z,Zs,Ms,Len).
merge_([X|Xs],Y,Ys,Z,Zs,Ms,Len) :- X@<Y, X@<Z, !, Ms=[X|Ms2], merge_(Xs,Y,Ys,Z,Zs,Ms2,Len1), Len is Len1+1.
merge_([X|Xs],Y,Ys,Z,Zs,Ms,Len) :- Y@<Z, !, Ms=[Y|Ms2], merge_(Ys,X,Xs,Z,Zs,Ms2,Len1), Len is Len1+1.
merge_([X|Xs],Y,Ys,Z,Zs,[Z|Ms2],Len) :- merge_(Zs,X,Xs,Y,Ys,Ms2,Len1), Len is Len1+1.

merge(Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Vs, Ws, Zs1, LenZs1), merge(Xs, Ys, Zs2, LenZs2), merge0(Zs1,Zs2,Zs), LenZs is LenZs1+LenZs2.
merge(Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Us, Vs, Ws, Zs1, LenZs1), merge(Xs, Ys, Zs2, LenZs2), merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Ts, Us, Vs, Zs1, LenZs1), merge(Ws, Xs, Ys, Zs2, LenZs2), merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Ss, Ts, Us, Vs, Zs1, LenZs1), merge(Ws, Xs, Ys, Zs2, LenZs2), merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Rs, Ss, Ts, Us, Zs1, LenZs1), merge(Vs, Ws, Xs, Ys, Zs2, LenZs2), merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Qs, Rs, Ss, Ts, Us, Zs1, LenZs1), merge(Vs, Ws, Xs, Ys, Zs2, LenZs2), 
    merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Ps, Qs, Rs, Ss, Ts, Zs1, LenZs1), merge(Us, Vs, Ws, Xs, Ys, Zs2, LenZs2), 
    merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Zs1, LenZs1), merge(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs2, LenZs2), 
    merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.
merge(Fs, Gs, Hs, Is, Js, Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs, LenZs) :- 
    merge(Fs, Gs, Hs, Is, Js, Ks, Ls, Ms, Ns, Os, Zs1, LenZs1), merge(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs2, LenZs2),
    merge0(Zs1, Zs2, Zs), LenZs is LenZs1+LenZs2.


%append([],Ys,Ys).
%append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).
%
%append(Ws, Xs, Ys, Zs) :- append(Xs, Ys, Zs1), append(Ws, Zs1, Zs).
%append(Vs, Ws, Xs, Ys, Zs) :- append(Ws, Xs, Ys, Zs1), append(Vs, Zs1, Zs).
%append(Us, Vs, Ws, Xs, Ys, Zs) :- append(Ws, Xs, Ys, Zs1), append(Us, Vs, Zs1, Zs).
%append(Ts, Us, Vs, Ws, Xs, Ys, Zs) :- append(Vs, Ws, Xs, Ys, Zs1), append(Ts, Us, Zs1, Zs).
%append(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- append(Vs, Ws, Xs, Ys, Zs1), append(Ss, Ts, Us, Zs1, Zs).
%append(Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- append(Us, Vs, Ws, Xs, Ys, Zs1), append(Rs, Ss, Ts, Zs1, Zs).
%append(Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- append(Us, Vs, Ws, Xs, Ys, Zs1), append(Qs, Rs, Ss, Ts, Zs1, Zs).
%append(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- append(Ts, Us, Vs, Ws, Xs, Ys, Zs1), append(Ps, Qs, Rs, Ss, Zs1, Zs).
%append(Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
%    append(Us, Vs, Ws, Xs, Ys, Zs1), append(Ps, Qs, Rs, Ss, Ts, Zs1, Zs2), append(Ks, Ls, Ms, Ns, Os, Zs2, Zs).
%append(Fs, Gs, Hs, Is, Js, Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
%    append(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs1), append(Ls, Ms, Ns, Os, Ps, Qs, Rs, Zs1, Zs2), 
%    append(Fs, Gs, Hs, Is, Js, Ks, Zs2, Zs).


%u([X, Y],X,Y).
%u([Y, X],X,Y).
%
%u([X | YZ],X,Y,Z) :- u(YZ,Y,Z).
%u([Y | XZ],X,Y,Z) :- u(XZ,X,Z).
%u([Z | XY],X,Y,Z) :- u(XY,X,Y).
%
%u([W | XYZ],W,X,Y,Z) :- u(XYZ,X,Y,Z).
%u([X | WYZ],W,X,Y,Z) :- u(WYZ,W,Y,Z).
%u([Y | WXZ],W,X,Y,Z) :- u(WXZ,W,X,Z).
%u([Z | WXY],W,X,Y,Z) :- u(WXY,W,X,Y).
%
%u([V | WXYZ],V,W,X,Y,Z) :- u(WXYZ,W,X,Y,Z).
%u([W | VXYZ],V,W,X,Y,Z) :- u(VXYZ,V,X,Y,Z).
%u([X | VWYZ],V,W,X,Y,Z) :- u(VWYZ,V,W,Y,Z).
%u([Y | VWXZ],V,W,X,Y,Z) :- u(VWXZ,V,W,X,Z).
%u([Z | VWXY],V,W,X,Y,Z) :- u(VWXY,V,W,X,Y).
%
%u([U | VWXYZ],U,V,W,X,Y,Z) :- u(VWXYZ,V,W,X,Y,Z).
%u([V | UWXYZ],U,V,W,X,Y,Z) :- u(UWXYZ,U,W,X,Y,Z).
%u([W | UVXYZ],U,V,W,X,Y,Z) :- u(UVXYZ,U,V,X,Y,Z).
%u([X | UVWYZ],U,V,W,X,Y,Z) :- u(UVWYZ,U,V,W,Y,Z).
%u([Y | UVWXZ],U,V,W,X,Y,Z) :- u(UVWXZ,U,V,W,X,Z).
%u([Z | UVWXY],U,V,W,X,Y,Z) :- u(UVWXY,U,V,W,X,Y).

perm([X, Y],X,Y, RX,RY, RX,RY).
perm([Y, X],X,Y, RX,RY, RY,RX).

perm([X | YZ],X,Y,Z, RX,RY,RZ, RX,R1,R2) :- perm(YZ,Y,Z, RY,RZ, R1,R2).
perm([Y | XZ],X,Y,Z, RX,RY,RZ, RY,R1,R2) :- perm(XZ,X,Z, RX,RZ, R1,R2).
perm([Z | XY],X,Y,Z, RX,RY,RZ, RZ,R1,R2) :- perm(XY,X,Y, RX,RY, R1,R2).

perm([W | XYZ],W,X,Y,Z, RW,RX,RY,RZ, RW,R1,R2,R3) :- 
    perm(XYZ,X,Y,Z, RX,RY,RZ, R1,R2,R3).
perm([X | WYZ],W,X,Y,Z, RW,RX,RY,RZ, RX,R1,R2,R3) :-
    perm(WYZ,W,Y,Z, RW,RY,RZ, R1,R2,R3).
perm([Y | WXZ],W,X,Y,Z, RW,RX,RY,RZ, RY,R1,R2,R3) :-
    perm(WXZ,W,X,Z, RW,RX,RZ, R1,R2,R3).
perm([Z | WXY],W,X,Y,Z, RW,RX,RY,RZ, RZ,R1,R2,R3) :-
    perm(WXY,W,X,Y, RW,RX,RY, R1,R2,R3).

perm([V | WXYZ],V,W,X,Y,Z, RV,RW,RX,RY,RZ, RV,R1,R2,R3,R4) :- 
    perm(WXYZ,W,X,Y,Z, RW,RX,RY,RZ, R1,R2,R3,R4).
perm([W | VXYZ],V,W,X,Y,Z, RV,RW,RX,RY,RZ, RW,R1,R2,R3,R4) :-
    perm(VXYZ,V,X,Y,Z, RV,RX,RY,RZ, R1,R2,R3,R4).
perm([X | VWYZ],V,W,X,Y,Z, RV,RW,RX,RY,RZ, RX,R1,R2,R3,R4) :-
    perm(VWYZ,V,W,Y,Z, RV,RW,RY,RZ, R1,R2,R3,R4).
perm([Y | VWXZ],V,W,X,Y,Z, RV,RW,RX,RY,RZ, RY,R1,R2,R3,R4) :-
    perm(VWXZ,V,W,X,Z, RV,RW,RX,RZ, R1,R2,R3,R4).
perm([Z | VWXY],V,W,X,Y,Z, RV,RW,RX,RY,RZ, RZ,R1,R2,R3,R4) :-
    perm(VWXY,V,W,X,Y, RV,RW,RX,RY, R1,R2,R3,R4).

perm([U | VWXYZ],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RU,R1,R2,R3,R4,R5) :- 
    perm(VWXYZ,V,W,X,Y,Z, RV,RW,RX,RY,RZ, R1,R2,R3,R4,R5).
perm([V | UWXYZ],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RV,R1,R2,R3,R4,R5) :- 
    perm(UWXYZ,U,W,X,Y,Z, RU,RW,RX,RY,RZ, R1,R2,R3,R4,R5).
perm([W | UVXYZ],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RW,R1,R2,R3,R4,R5) :- 
    perm(UVXYZ,U,V,X,Y,Z, RU,RV,RX,RY,RZ, R1,R2,R3,R4,R5).
perm([X | UVWYZ],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RX,R1,R2,R3,R4,R5) :- 
    perm(UVWYZ,U,V,W,Y,Z, RU,RV,RW,RY,RZ, R1,R2,R3,R4,R5).
perm([Y | UVWXZ],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RY,R1,R2,R3,R4,R5) :- 
    perm(UVWXZ,U,V,W,X,Z, RU,RV,RW,RX,RZ, R1,R2,R3,R4,R5).
perm([Z | UVWXY],U,V,W,X,Y,Z, RU,RV,RW,RX,RY,RZ, RZ,R1,R2,R3,R4,R5) :- 
    perm(UVWXY,U,V,W,X,Y, RU,RV,RW,RX,RY, R1,R2,R3,R4,R5).

%replacef([],[],_,_).
%replacef([S | Ss],[D | Ds],SC,DC) :- 
%    replacel(S,D,SC,DC),
%    replacef(Ss,Ds,SC,DC).

replacel([],[],_,_).
replacel([d(US,AS) | Ss],[d(UD,AD) | Ds],SC,DC) :- 
    repl_attrlist(US,UD1,SC,DC), sort(UD1,UD), repl_atom(AS,AD,SC,DC), 
    replacel(Ss,Ds,SC,DC).

repl_attrlist([],[],_,_).
repl_attrlist([S | Ss],[D | Ds],SC,DC) :- 
    repl_atom(S,D,SC,DC), 
    repl_attrlist(Ss,Ds,SC,DC).

repl_atom(S,S,[],[]) :- !.
repl_atom(S,D,[S | _],[D | _]) :- !.
repl_atom(S,D,[_ | SCs],[_ | DCs]) :- !, repl_atom(S,D,SCs,DCs).


tvalid(Y,A,B,LYA,LYB) :- 
    (member(d(Y,A),LYA) -> member(d(Y,B),LYB); true).

case1([],_A,100,PA) :- PA>0.
case1([d([],A)],A,0,0).

case1fc([c(LenFL0),FL0],A,RA) :- 
    case1(FL0,A,RA,RA),
    length(FL0,LenFL0).

cases1fc(Ss,N,A,RA) :- 
    findall(S,case1fc(S,A,RA),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases1fc([],[],_,_).
reducecases1fc([[Crec|Ls] | Ss],RSs,X,RX) :- 
    firstcasec(Ss,Crec),
    similarcasefcfound(Ss,Crec,Ls,[X],[X]),
    !, reducecases1fc(Ss,RSs,X,RX).
reducecases1fc([S | Ss],[S | RSs],X,RX) :- 
    reducecases1fc(Ss,RSs,X,RX).
    

case2(LAB,LA0,LB0,A,B,RA,RB,PA,PB) :- 
    case1(LA0,A,RAm,PA),
    case1(LB0,B,RBm,PB),

    (RBm<100 -> (tvalid([],A,B,LA0,LB0), LAB=[d([A],B) | LAB1], RB=RBm)
     ; ((PB=<1, tvalid([],A,B,LA0,LB0), LAB=[d([A],B) | LAB1], RB=1); (LAB=LAB1, RB=100))),
    (RAm<100 -> (tvalid([],B,A,LB0,LA0), LAB1=[d([B],A)], RA=RAm)
     ; ((PA=<1, tvalid([],B,A,LB0,LA0), LAB1=[d([B],A)], RA=1); (LAB1=[], RA=100))).


case2fc([c(LenFL1,LenFL0),FL1,FL0],A,B,RA,RB) :- 
    case2(FL1,LA0,LB0,A,B,RA,RB,RA,RB),
    length(FL1,LenFL1), merge(LA0,LB0,FL0,LenFL0).

cases2fc(Ss,N,A,B,RA,RB) :- 
    findall(S,case2fc(S,A,B,RA,RB),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases2fc([],[],_,_,_,_).
reducecases2fc([[Crec|Ls] | Ss],RSs,X,Y,RX,RY) :- 
    firstcasec(Ss,Crec),
    perm(PermXY,X,Y,RX,RY,RX,RY),
    similarcasefcfound(Ss,Crec,Ls,[X,Y],PermXY), 
    !, reducecases2fc(Ss,RSs,X,Y,RX,RY).
reducecases2fc([S | Ss],[S | RSs],X,Y,RX,RY) :- 
    reducecases2fc(Ss,RSs,X,Y,RX,RY).
    

case3(LABC,LAB,LAC,LBC,LA0,LB0,LC0,A,B,C,RA,RB,RC,PA,PB,PC) :- 

    case2(LAB,LA0,LB0,A,B,RA1,RB1,PA,PB), 
    case2(LAC,LA0,LC0,A,C,RA2,RC1,PA,PC), 
    case2(LBC,LB0,LC0,B,C,RB2,RC2,PB,PC),

    RAm is min(RA1,RA2), RBm is min(RB1,RB2), RCm is min(RC1,RC2),
    
    (RCm<100 -> (tvalid([A],B,C,LAB,LAC), tvalid([B],A,C,LAB,LBC), LABC=[d([A,B],C) | LABC1], RC=RCm)
     ; ((PC=<2, tvalid([A],B,C,LAB,LAC), tvalid([B],A,C,LAB,LBC), LABC=[d([A,B],C) | LABC1], RC=2); (LABC=LABC1, RC=100))),
    (RBm<100 -> (tvalid([A],C,B,LAC,LAB), tvalid([C],A,B,LAC,LBC), LABC1=[d([A,C],B) | LABC2], RB=RBm)
     ; ((PB=<2, tvalid([A],C,B,LAC,LAB), tvalid([C],A,B,LAC,LBC), LABC1=[d([A,C],B) | LABC2], RB=2); (LABC1=LABC2, RB=100))),
    (RAm<100 -> (tvalid([B],C,A,LBC,LAB), tvalid([C],B,A,LBC,LAC), LABC2=[d([B,C],A)], RA=RAm)
     ; ((PA=<2, tvalid([B],C,A,LBC,LAB), tvalid([C],B,A,LBC,LAC), LABC2=[d([B,C],A)], RA=2); (LABC2=[], RA=100))).

case3fc([c(LenFL2,LenFL1,LenFL0),FL2,FL1,FL0],A,B,C,RA,RB,RC) :- 
    case3(FL2,LAB,LAC,LBC,LA0,LB0,LC0,A,B,C,RA,RB,RC,RA,RB,RC),
    length(FL2,LenFL2), merge(LAB,LAC,LBC,FL1,LenFL1), merge(LA0,LB0,LC0,FL0,LenFL0).

cases3fc(Ss,N,A,B,C,RA,RB,RC) :- 
    findall(S,case3fc(S,A,B,C,RA,RB,RC),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases3fc([],[],_,_,_,_,_,_).
reducecases3fc([[Crec|Ls] | Ss],RSs,X,Y,Z,RX,RY,RZ) :- 
    firstcasec(Ss,Crec),
    perm(PermXYZ,X,Y,Z,RX,RY,RZ,RX,RY,RZ),
    similarcasefcfound(Ss,Crec,Ls,[X,Y,Z],PermXYZ), 
    !, reducecases3fc(Ss,RSs,X,Y,Z,RX,RY,RZ).
reducecases3fc([S | Ss],[S | RSs],X,Y,Z,RX,RY,RZ) :- 
    reducecases3fc(Ss,RSs,X,Y,Z,RX,RY,RZ).


case4(LABCD,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
      LA0,LB0,LC0,LD0,
      A,B,C,D,RA,RB,RC,RD,PA,PB,PC,PD) :- 
    case3(LABC,LAB,LAC,LBC,LA0,LB0,LC0,A,B,C,RA1,RB1,RC1,PA,PB,PC), 
    case3(LABD,LAB,LAD,LBD,LA0,LB0,LD0,A,B,D,RA2,RB2,RD1,PA,PB,PD),
    case3(LACD,LAC,LAD,LCD,LA0,LC0,LD0,A,C,D,RA3,RC2,RD2,PA,PC,PD), 
    case3(LBCD,LBC,LBD,LCD,LB0,LC0,LD0,B,C,D,RB3,RC3,RD3,PB,PC,PD),

    RAm is min(RA1,min(RA2,RA3)), RBm is min(RB1,min(RB2,RB3)),
    RCm is min(RC1,min(RC2,RC3)), RDm is min(RD1,min(RD2,RD3)),

    (RDm<100 -> (tvalid([A,B],C,D,LABC,LABD), tvalid([A,C],B,D,LABC,LACD), tvalid([B,C],A,D,LABC,LBCD), 
		 LABCD=[d([A,B,C],D) | LABCD1], RD=RDm)
     ; ((PD=<3, tvalid([A,B],C,D,LABC,LABD), tvalid([A,C],B,D,LABC,LACD), tvalid([B,C],A,D,LABC,LBCD), 
	 LABCD=[d([A,B,C],D) | LABCD1], RD=3); 
	(LABCD=LABCD1, RD=100))),
    (RCm<100 -> (tvalid([A,B],D,C,LABD,LABC), tvalid([A,D],B,C,LABD,LACD), tvalid([B,D],A,C,LABD,LBCD), 
		 LABCD1=[d([A,B,D],C) | LABCD2], RC=RCm)
     ; ((PC=<3, tvalid([A,B],D,C,LABD,LABC), tvalid([A,D],B,C,LABD,LACD), tvalid([B,D],A,C,LABD,LBCD), 
	 LABCD1=[d([A,B,D],C) | LABCD2], RC=3); 
	(LABCD1=LABCD2, RC=100))),
    (RBm<100 -> (tvalid([A,C],D,B,LACD,LABC), tvalid([A,D],C,B,LACD,LABD), tvalid([C,D],A,B,LACD,LBCD), 
		 LABCD2=[d([A,C,D],B) | LABCD3], RB=RBm)
     ; ((PB=<3, tvalid([A,C],D,B,LACD,LABC), tvalid([A,D],C,B,LACD,LABD), tvalid([C,D],A,B,LACD,LBCD), 
	 LABCD2=[d([A,C,D],B) | LABCD3], RB=3); 
	(LABCD2=LABCD3, RB=100))),
    (RAm<100 -> (tvalid([B,C],D,A,LBCD,LABC), tvalid([B,D],C,A,LBCD,LABD), tvalid([C,D],B,A,LBCD,LACD), 
		 LABCD3=[d([B,C,D],A)], RA=RAm)
     ; ((PA=<3, tvalid([B,C],D,A,LBCD,LABC), tvalid([B,D],C,A,LBCD,LABD), tvalid([C,D],B,A,LBCD,LACD), 
	 LABCD3=[d([B,C,D],A)], RA=3); 
	(LABCD3=[], RA=100))).

case4fc([c(LenFL3,LenFL2,LenFL1,LenFL0),FL3,FL2,FL1,FL0],A,B,C,D,RA,RB,RC,RD) :- 
    case4(FL3,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
	  LA0,LB0,LC0,LD0,
	  A,B,C,D,RA,RB,RC,RD,RA,RB,RC,RD),
    length(FL3,LenFL3), merge(LABC,LABD,LACD,LBCD,FL2,LenFL2), merge(LAB,LAC,LAD,LBC,LBD,LCD,FL1,LenFL1),
    merge(LA0,LB0,LC0,LD0,FL0,LenFL0).

cases4fc(Ss,N,A,B,C,D,RA,RB,RC,RD) :- 
    findall(S,case4fc(S,A,B,C,D,RA,RB,RC,RD),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases4fc([],[],_,_,_,_,_,_,_,_).
reducecases4fc([[Crec|Ls] | Ss],RSs,W,X,Y,Z,RW,RX,RY,RZ) :- 
    firstcasec(Ss,Crec),
    perm(PermWXYZ,W,X,Y,Z,RW,RX,RY,RZ,RW,RX,RY,RZ),
    similarcasefcfound(Ss,Crec,Ls,[W,X,Y,Z],PermWXYZ), 
    !, reducecases4fc(Ss,RSs,W,X,Y,Z,RW,RX,RY,RZ).
reducecases4fc([S | Ss],[S | RSs],W,X,Y,Z,RW,RX,RY,RZ) :- 
    reducecases4fc(Ss,RSs,W,X,Y,Z,RW,RX,RY,RZ).


case5(LABCDE,LABCD,LABCE,LABDE,LACDE,LBCDE,
      LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
      LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
      LA0,LB0,LC0,LD0,LE0,
      A,B,C,D,E,RA,RB,RC,RD,RE,PA,PB,PC,PD,PE) :- 
    case4(LABCD,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
	  LA0,LB0,LC0,LD0,
	  A,B,C,D,RA1,RB1,RC1,RD1,PA,PB,PC,PD),
    case4(LABCE,LABC,LABE,LACE,LBCE,LAB,LAC,LAE,LBC,LBE,LCE,
	  LA0,LB0,LC0,LE0,
	  A,B,C,E,RA2,RB2,RC2,RE1,PA,PB,PC,PE),
    case4(LABDE,LABD,LABE,LADE,LBDE,LAB,LAD,LAE,LBD,LBE,LDE,
	  LA0,LB0,LD0,LE0,
	  A,B,D,E,RA3,RB3,RD2,RE2,PA,PB,PD,PE),
    case4(LACDE,LACD,LACE,LADE,LCDE,LAC,LAD,LAE,LCD,LCE,LDE,
	  LA0,LC0,LD0,LE0,
	  A,C,D,E,RA4,RC3,RD3,RE3,PA,PC,PD,PE),
    case4(LBCDE,LBCD,LBCE,LBDE,LCDE,LBC,LBD,LBE,LCD,LCE,LDE,
	  LB0,LC0,LD0,LE0,
	  B,C,D,E,RB4,RC4,RD4,RE4,PB,PC,PD,PE),

    RAm is min(RA1,min(RA2,min(RA3,RA4))), RBm is min(RB1,min(RB2,min(RB3,RB4))),
    RCm is min(RC1,min(RC2,min(RC3,RC4))), RDm is min(RD1,min(RD2,min(RD3,RD4))),
    REm is min(RE1,min(RE2,min(RE3,RE4))),

    (REm<100 -> (tvalid([A,B,C],D,E,LABCD,LABCE), tvalid([A,B,D],C,E,LABCD,LABDE), 
		 tvalid([A,C,D],B,E,LABCD,LACDE), tvalid([B,C,D],A,E,LABCD,LBCDE),
		 LABCDE=[d([A,B,C,D],E) | LABCDE1], RE=REm)
     ;((PE=<4, tvalid([A,B,C],D,E,LABCD,LABCE), tvalid([A,B,D],C,E,LABCD,LABDE), 
	       tvalid([A,C,D],B,E,LABCD,LACDE), tvalid([B,C,D],A,E,LABCD,LBCDE),
	       LABCDE=[d([A,B,C,D],E) | LABCDE1], RE=4); 
       (LABCDE=LABCDE1, RE=100))),
    (RDm<100 -> (tvalid([A,B,C],E,D,LABCE,LABCD), tvalid([A,B,E],C,D,LABCE,LABDE), 
		 tvalid([A,C,E],B,D,LABCE,LACDE), tvalid([B,C,E],A,D,LABCE,LBCDE),
		 LABCDE1=[d([A,B,C,E],D) | LABCDE2], RD=RDm)
     ;((PD=<4, tvalid([A,B,C],E,D,LABCE,LABCD), tvalid([A,B,E],C,D,LABCE,LABDE), 
	       tvalid([A,C,E],B,D,LABCE,LACDE), tvalid([B,C,E],A,D,LABCE,LBCDE),
	       LABCDE1=[d([A,B,C,E],D) | LABCDE2], RD=4); 
       (LABCDE1=LABCDE2, RD=100))),
    (RCm<100 -> (tvalid([A,B,D],E,C,LABDE,LABCD), tvalid([A,B,E],D,C,LABDE,LABCE), 
		 tvalid([A,D,E],B,C,LABDE,LACDE), tvalid([B,D,E],A,C,LABDE,LBCDE),
		 LABCDE2=[d([A,B,D,E],C) | LABCDE3], RC=RCm)
     ;((PC=<4, tvalid([A,B,D],E,C,LABDE,LABCD), tvalid([A,B,E],D,C,LABDE,LABCE), 
	       tvalid([A,D,E],B,C,LABDE,LACDE), tvalid([B,D,E],A,C,LABDE,LBCDE),
	       LABCDE2=[d([A,B,D,E],C) | LABCDE3], RC=4); 
       (LABCDE2=LABCDE3, RC=100))),
    (RBm<100 -> (tvalid([A,C,D],E,B,LACDE,LABCD), tvalid([A,C,E],D,B,LACDE,LABCE), 
		 tvalid([A,D,E],C,B,LACDE,LABDE), tvalid([C,D,E],A,B,LACDE,LBCDE),
		 LABCDE3=[d([A,C,D,E],B) | LABCDE4], RB=RBm)
     ;((PB=<4, tvalid([A,C,D],E,B,LACDE,LABCD), tvalid([A,C,E],D,B,LACDE,LABCE), 
	       tvalid([A,D,E],C,B,LACDE,LABDE), tvalid([C,D,E],A,B,LACDE,LBCDE),
	       LABCDE3=[d([A,C,D,E],B) | LABCDE4], RB=4); 
       (LABCDE3=LABCDE4, RB=100))),
    (RAm<100 -> (tvalid([B,C,D],E,A,LBCDE,LABCD), tvalid([B,C,E],D,A,LBCDE,LABCE), 
		 tvalid([B,D,E],C,A,LBCDE,LABDE), tvalid([C,D,E],B,A,LBCDE,LACDE),
		 LABCDE4=[d([B,C,D,E],A)], RA=RAm)
     ;((PA=<4, tvalid([B,C,D],E,A,LBCDE,LABCD), tvalid([B,C,E],D,A,LBCDE,LABCE), 
	       tvalid([B,D,E],C,A,LBCDE,LABDE), tvalid([C,D,E],B,A,LBCDE,LACDE),
	       LABCDE4=[d([B,C,D,E],A)], RA=4); 
       (LABCDE4=[], RA=100))).

case5fc([c(LenFL4,LenFL3,LenFL2,LenFL1,LenFL0),FL4,FL3,FL2,FL1,FL0],
	A,B,C,D,E,RA,RB,RC,RD,RE) :- 
    case5(FL4,LABCD,LABCE,LABDE,LACDE,LBCDE,
      LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
      LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
      LA0,LB0,LC0,LD0,LE0,
	  A,B,C,D,E,RA,RB,RC,RD,RE,RA,RB,RC,RD,RE),
    length(FL4,LenFL4),
    merge(LABCD,LABCE,LABDE,LACDE,LBCDE,FL3,LenFL3), 
    merge(LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,FL2,LenFL2), 
    merge(LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,FL1,LenFL1),
    merge(LA0,LB0,LC0,LD0,LE0,FL0,LenFL0).

cases5fc(Ss,N,A,B,C,D,E,RA,RB,RC,RD,RE) :- 
    findall(S,case5fc(S,A,B,C,D,E,RA,RB,RC,RD,RE),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases5fc([],[],_,_,_,_,_,_,_,_,_,_).
reducecases5fc([[Crec|Ls] | Ss],RSs,V,W,X,Y,Z,RV,RW,RX,RY,RZ) :- 
    firstcasec(Ss,Crec),
    perm(PermVWXYZ,V,W,X,Y,Z,RV,RW,RX,RY,RZ,RV,RW,RX,RY,RZ),
    similarcasefcfound(Ss,Crec,Ls,[V,W,X,Y,Z],PermVWXYZ), 
    !, reducecases5fc(Ss,RSs,V,W,X,Y,Z,RV,RW,RX,RY,RZ).
reducecases5fc([S | Ss],[S | RSs],V,W,X,Y,Z,RV,RW,RX,RY,RZ) :- 
    reducecases5fc(Ss,RSs,V,W,X,Y,Z,RV,RW,RX,RY,RZ).


case6(LABCDEF, LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,
	  LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
	  LA0,LB0,LC0,LD0,LE0,LF0,
      A,B,C,D,E,F,RA,RB,RC,RD,RE,RF,PA,PB,PC,PD,PE,PF) :- 
    case5(LABCDE, LABCD,LABCE,LABDE,LACDE,LBCDE,
	  LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
	  LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
	  LA0,LB0,LC0,LD0,LE0,
	  A,B,C,D,E,RA1,RB1,RC1,RD1,RE1,PA,PB,PC,PD,PE),
    case5(LABCDF, LABCD,LABCF,LABDF,LACDF,LBCDF,
	  LABC,LABD,LABF,LACD,LACF,LADF,LBCD,LBCF,LBDF,LCDF,
	  LAB,LAC,LAD,LAF,LBC,LBD,LBF,LCD,LCF,LDF,
	  LA0,LB0,LC0,LD0,LF0,
	  A,B,C,D,F,RA2,RB2,RC2,RD2,RF1,PA,PB,PC,PD,PF),
    case5(LABCEF,LABCE,LABCF,LABEF,LACEF,LBCEF,
	  LABC,LABE,LABF,LACE,LACF,LAEF,LBCE,LBCF,LBEF,LCEF,
	  LAB,LAC,LAE,LAF,LBC,LBE,LBF,LCE,LCF,LEF,
	  LA0,LB0,LC0,LE0,LF0,
	  A,B,C,E,F,RA3,RB3,RC3,RE2,RF2,PA,PB,PC,PE,PF),
    case5(LABDEF,LABDE,LABDF,LABEF,LADEF,LBDEF,
	  LABD,LABE,LABF,LADE,LADF,LAEF,LBDE,LBDF,LBEF,LDEF,
	  LAB,LAD,LAE,LAF,LBD,LBE,LBF,LDE,LDF,LEF,
	  LA0,LB0,LD0,LE0,LF0,
	  A,B,D,E,F,RA4,RB4,RD3,RE3,RF3,PA,PB,PD,PE,PF),
    case5(LACDEF,LACDE,LACDF,LACEF,LADEF,LCDEF,
	  LACD,LACE,LACF,LADE,LADF,LAEF,LCDE,LCDF,LCEF,LDEF,
	  LAC,LAD,LAE,LAF,LCD,LCE,LCF,LDE,LDF,LEF,
	  LA0,LC0,LD0,LE0,LF0,
	  A,C,D,E,F,RA5,RC4,RD4,RE4,RF4,PA,PC,PD,PE,PF),
    case5(LBCDEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
	  LB0,LC0,LD0,LE0,LF0,
	  B,C,D,E,F,RB5,RC5,RD5,RE5,RF5,PB,PC,PD,PE,PF),

    RAm is min(RA1,min(RA2,min(RA3,min(RA4,RA5)))), RBm is min(RB1,min(RB2,min(RB3,min(RB4,RB5)))),
    RCm is min(RC1,min(RC2,min(RC3,min(RC4,RC5)))), RDm is min(RD1,min(RD2,min(RD3,min(RD4,RD5)))),
    REm is min(RE1,min(RE2,min(RE3,min(RE4,RE5)))), RFm is min(RF1,min(RF2,min(RF3,min(RF4,RF5)))),

    (RFm<100 -> (tvalid([A,B,C,D],E,F,LABCDE,LABCDF), 
		 tvalid([A,B,C,E],D,F,LABCDE,LABCEF), tvalid([A,B,D,E],C,F,LABCDE,LABDEF), 
		 tvalid([A,C,D,E],B,F,LABCDE,LACDEF), tvalid([B,C,D,E],A,F,LABCDE,LBCDEF),
		 LABCDEF=[d([A,B,C,D,E],F) | LABCDEF1], RF=RFm)
     ;((PF=<5, tvalid([A,B,C,D],E,F,LABCDE,LABCDF), 
	       tvalid([A,B,C,E],D,F,LABCDE,LABCEF), tvalid([A,B,D,E],C,F,LABCDE,LABDEF), 
	       tvalid([A,C,D,E],B,F,LABCDE,LACDEF), tvalid([B,C,D,E],A,F,LABCDE,LBCDEF),
	       LABCDEF=[d([A,B,C,D,E],F) | LABCDEF1], RF=5); 
       (LABCDEF=LABCDEF1, RF=100))),
    (REm<100 -> (tvalid([A,B,C,D],F,E,LABCDF,LABCDE), 
		 tvalid([A,B,C,F],D,E,LABCDF,LABCEF), tvalid([A,B,D,F],C,E,LABCDF,LABDEF), 
		 tvalid([A,C,D,F],B,E,LABCDF,LACDEF), tvalid([B,C,D,F],A,E,LABCDF,LBCDEF),
		 LABCDEF1=[d([A,B,C,D,F],E) | LABCDEF2], RE=REm)
     ;((PE=<5, tvalid([A,B,C,D],F,E,LABCDF,LABCDE), 
	       tvalid([A,B,C,F],D,E,LABCDF,LABCEF), tvalid([A,B,D,F],C,E,LABCDF,LABDEF), 
	       tvalid([A,C,D,F],B,E,LABCDF,LACDEF), tvalid([B,C,D,F],A,E,LABCDF,LBCDEF),
	       LABCDEF1=[d([A,B,C,D,F],E) | LABCDEF2], RE=5); 
       (LABCDEF1=LABCDEF2, RE=100))),
    (RDm<100 -> (tvalid([A,B,C,E],F,D,LABCEF,LABCDE), 
		 tvalid([A,B,C,F],E,D,LABCEF,LABCDF), tvalid([A,B,E,F],C,D,LABCEF,LABDEF), 
		 tvalid([A,C,E,F],B,D,LABCEF,LACDEF), tvalid([B,C,E,F],A,D,LABCEF,LBCDEF),
		 LABCDEF2=[d([A,B,C,E,F],D) | LABCDEF3], RD=RDm)
     ;((PD=<5, tvalid([A,B,C,E],F,D,LABCEF,LABCDE), 
	       tvalid([A,B,C,F],E,D,LABCEF,LABCDF), tvalid([A,B,E,F],C,D,LABCEF,LABDEF), 
	       tvalid([A,C,E,F],B,D,LABCEF,LACDEF), tvalid([B,C,E,F],A,D,LABCEF,LBCDEF),
	       LABCDEF2=[d([A,B,C,E,F],D) | LABCDEF3], RD=5); 
       (LABCDEF2=LABCDEF3, RD=100))),
    (RCm<100 -> (tvalid([A,B,D,E],F,C,LABDEF,LABCDE),
		 tvalid([A,B,D,F],E,C,LABDEF,LABCDF), tvalid([A,B,E,F],D,C,LABDEF,LABCEF), 
		 tvalid([A,D,E,F],B,C,LABDEF,LACDEF), tvalid([B,D,E,F],A,C,LABDEF,LBCDEF),
		 LABCDEF3=[d([A,B,D,E,F],C) | LABCDEF4], RC=RCm)
     ;((PC=<5, tvalid([A,B,D,E],F,C,LABDEF,LABCDE),
	       tvalid([A,B,D,F],E,C,LABDEF,LABCDF), tvalid([A,B,E,F],D,C,LABDEF,LABCEF), 
	       tvalid([A,D,E,F],B,C,LABDEF,LACDEF), tvalid([B,D,E,F],A,C,LABDEF,LBCDEF),
	       LABCDEF3=[d([A,B,D,E,F],C) | LABCDEF4], RC=5); 
       (LABCDEF3=LABCDEF4, RC=100))),
    (RBm<100 -> (tvalid([A,C,D,E],F,B,LACDEF,LABCDE),
		 tvalid([A,C,D,F],E,B,LACDEF,LABCDF), tvalid([A,C,E,F],D,B,LACDEF,LABCEF), 
		 tvalid([A,D,E,F],C,B,LACDEF,LABDEF), tvalid([C,D,E,F],A,B,LACDEF,LBCDEF),
		 LABCDEF4=[d([A,C,D,E,F],B) | LABCDEF5], RB=RBm)
     ;((PB=<5, tvalid([A,C,D,E],F,B,LACDEF,LABCDE),
	       tvalid([A,C,D,F],E,B,LACDEF,LABCDF), tvalid([A,C,E,F],D,B,LACDEF,LABCEF), 
	       tvalid([A,D,E,F],C,B,LACDEF,LABDEF), tvalid([C,D,E,F],A,B,LACDEF,LBCDEF),
	       LABCDEF4=[d([A,C,D,E,F],B) | LABCDEF5], RB=5); 
       (LABCDEF4=LABCDEF5, RB=100))),
    (RAm<100 -> (tvalid([B,C,D,E],F,A,LBCDEF,LABCDE), 
		 tvalid([B,C,D,F],E,A,LBCDEF,LABCDF), tvalid([B,C,E,F],D,A,LBCDEF,LABCEF), 
		 tvalid([B,D,E,F],C,A,LBCDEF,LABDEF), tvalid([C,D,E,F],B,A,LBCDEF,LACDEF),
		 LABCDEF5=[d([B,C,D,E,F],A)], RA=RAm)
     ;((PA=<5, tvalid([B,C,D,E],F,A,LBCDEF,LABCDE), 
	       tvalid([B,C,D,F],E,A,LBCDEF,LABCDF), tvalid([B,C,E,F],D,A,LBCDEF,LABCEF), 
	       tvalid([B,D,E,F],C,A,LBCDEF,LABDEF), tvalid([C,D,E,F],B,A,LBCDEF,LACDEF),
	       LABCDEF5=[d([B,C,D,E,F],A)], RA=5); 
       (LABCDEF5=[], RA=100))).

case6fc([c(LenFL5,LenFL4,LenFL3,LenFL2,LenFL1,LenFL0),FL5,FL4,FL3,FL2,FL1,FL0],
	A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :- 
    case6(FL5, LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,
	  LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
	  LA0,LB0,LC0,LD0,LE0,LF0,
	  A,B,C,D,E,F,RA,RB,RC,RD,RE,RF,RA,RB,RC,RD,RE,RF),
    length(FL5,LenFL5), 
    merge(LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,FL4,LenFL4),
    merge(LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,FL3,LenFL3), 
    merge(LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,FL2,LenFL2),
    merge(LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,FL1,LenFL1),
    merge(LA0,LB0,LC0,LD0,LE0,LF0,FL0,LenFL0).

cases6fc(Ss,N,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :- 
    findall(S,case6fc(S,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF),Ss1),
    length(Ss1,N),sort(Ss1,Ss).

reducecases6fc([],[],_,_,_,_,_,_,_,_,_,_,_,_).
reducecases6fc([[Crec|Ls] | Ss],RSs,U,V,W,X,Y,Z,RU,RV,RW,RX,RY,RZ) :- 
    firstcasec(Ss,Crec),
    perm(PermUVWXYZ,U,V,W,X,Y,Z,RU,RV,RW,RX,RY,RZ,RU,RV,RW,RX,RY,RZ),
    similarcasefcfound(Ss,Crec,Ls,[U,V,W,X,Y,Z],PermUVWXYZ), 
    !, reducecases6fc(Ss,RSs,U,V,W,X,Y,Z,RU,RV,RW,RX,RY,RZ).
reducecases6fc([S | Ss],[S | RSs],U,V,W,X,Y,Z,RU,RV,RW,RX,RY,RZ) :- 
    reducecases6fc(Ss,RSs,U,V,W,X,Y,Z,RU,RV,RW,RX,RY,RZ).
