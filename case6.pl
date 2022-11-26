
member(X,[X | _]).
member(X,[_ | Ys]) :- member(X,Ys).

memberdep(d([X],A),[d([X],A) | _]).
memberdep(d([X,Y],A),[d([U,V],A) | _]) :- u([U,V],X,Y).
memberdep(d([X,Y,Z],A),[d([U,V,W],A) | _]) :- u([U,V,W],X,Y,Z).
memberdep(d([W,X,Y,Z],A),[d([S,T,U,V],A) | _]) :- u([S,T,U,V],W,X,Y,Z).
memberdep(d([V1,W,X,Y,Z],A),[d([R,S,T,U,V],A) | _]) :- u([R,S,T,U,V],V1,W,X,Y,Z).
memberdep(D,[_ | Ls]) :- memberdep(D,Ls).

memberdepexcl(d([X],A),[d([X],A) | Ls],Ls) :- !.
memberdepexcl(d([X,Y],A),[d([U,V],A) | Ls],Ls) :- u([U,V],X,Y), !.
memberdepexcl(d([X,Y,Z],A),[d([U,V,W],A) | Ls],Ls) :- u([U,V,W],X,Y,Z), !.
memberdepexcl(d([W,X,Y,Z],A),[d([S,T,U,V],A) | Ls],Ls) :- 
    u([S,T,U,V],W,X,Y,Z), !.
memberdepexcl(d([V1,W,X,Y,Z],A),[d([R,S,T,U,V],A) | Ls],Ls) :- 
    u([R,S,T,U,V],V1,W,X,Y,Z), !.
memberdepexcl(D,[L1 | Ls],[L1 | LsE]) :- memberdepexcl(D,Ls,LsE).

similardeps([],[]).
similardeps([D | L1s],L2) :- memberdepexcl(D,L2,L2s), similardeps(L1s,L2s).

similarcasef([],[]).
similarcasef([L | Ls],[L2 | L2s]) :- length(L,LN), length(L2,LN), similardeps(L,L2), similarcasef(Ls,L2s).


append([],Ys,Ys).
append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).

append(Ws, Xs, Ys, Zs) :- append(Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Vs, Ws, Xs, Ys, Zs) :- append(Vs, Ws, Zs1), append(Xs, Ys, Zs2), append(Zs1,Zs2,Zs).
append(Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Us, Vs, Ws, Zs1), append(Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ts, Us, Vs, Zs1), append(Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ss, Ts, Us, Vs, Zs1), append(Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Rs, Ss, Ts, Us, Zs1), append(Vs, Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Qs, Rs, Ss, Ts, Us, Zs1), append(Vs, Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ps, Qs, Rs, Ss, Ts, Zs1), append(Us, Vs, Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Zs1), append(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs2), append(Zs1, Zs2, Zs).
append(Fs, Gs, Hs, Is, Js, Ks, Ls, Ms, Ns, Os, Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Fs, Gs, Hs, Is, Js, Ks, Ls, Ms, Ns, Os, Zs1), append(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs2), 
    append(Zs1, Zs2, Zs).


min(X,Y,M) :- X>Y, !, M=Y.
min(X,_,X).

min(X,Y,Z,M) :- min(X,Y,M1), min(M1,Z,M).
min(W,X,Y,Z,M) :- min(W,X,M1), min(Y,Z,M2), min(M1,M2,M).
min(V,W,X,Y,Z,M) :- min(V,W,X,M1), min(Y,Z,M2), min(M1,M2,M).


u([X, Y],X,Y).
u([Y, X],X,Y).

u([X | YZ],X,Y,Z) :- u(YZ,Y,Z).
u([Y | XZ],X,Y,Z) :- u(XZ,X,Z).
u([Z | XY],X,Y,Z) :- u(XY,X,Y).

u([W | XYZ],W,X,Y,Z) :- u(XYZ,X,Y,Z).
u([X | WYZ],W,X,Y,Z) :- u(WYZ,W,Y,Z).
u([Y | WXZ],W,X,Y,Z) :- u(WXZ,W,X,Z).
u([Z | WXY],W,X,Y,Z) :- u(WXY,W,X,Y).

u([V | WXYZ],V,W,X,Y,Z) :- u(WXYZ,W,X,Y,Z).
u([W | VXYZ],V,W,X,Y,Z) :- u(VXYZ,V,X,Y,Z).
u([X | VWYZ],V,W,X,Y,Z) :- u(VWYZ,V,W,Y,Z).
u([Y | VWXZ],V,W,X,Y,Z) :- u(VWXZ,V,W,X,Z).
u([Z | VWXY],V,W,X,Y,Z) :- u(VWXY,V,W,X,Y).

u([U | VWXYZ],U,V,W,X,Y,Z) :- u(VWXYZ,V,W,X,Y,Z).
u([V | UWXYZ],U,V,W,X,Y,Z) :- u(UWXYZ,U,W,X,Y,Z).
u([W | UVXYZ],U,V,W,X,Y,Z) :- u(UVXYZ,U,V,X,Y,Z).
u([X | UVWYZ],U,V,W,X,Y,Z) :- u(UVWYZ,U,V,W,Y,Z).
u([Y | UVWXZ],U,V,W,X,Y,Z) :- u(UVWXZ,U,V,W,X,Z).
u([Z | UVWXY],U,V,W,X,Y,Z) :- u(UVWXY,U,V,W,X,Y).


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


case2([],_,_,100,100,_,_).
case2([d([A],B)],A,B,100,1,_,PB) :- PB=<1.
case2([d([B],A)],A,B,1,100,PA,_) :- PA=<1.
case2([d([A],B), d([B],A)],A,B,1,1,PA,PB) :- PA=<1, PB=<1.

case2f([FL1],A,B,RA,RB) :- case2(FL1,A,B,RA,RB,RA,RB).

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

case2fr(RFL1,A,B,RA,RB) :- 
    cases5fr(RSs,_,A,B,RA,RB), 
    member([RFL1],RSs).

tvalid(Y,A,B,LYA,LYB) :-
    (memberdep(d(Y,A),LYA) -> memberdep(d(Y,B),LYB); true).

case3(LABC,LAB,LAC,LBC,A,B,C,RA,RB,RC,PA,PB,PC) :- 

    case2(LAB,A,B,RA1,RB1,PA,PB), 
    case2(LAC,A,C,RA2,RC1,PA,PC), 
    case2(LBC,B,C,RB2,RC2,PB,PC),

    min(RA1,RA2,RAm), min(RB1,RB2,RBm), min(RC1,RC2,RCm),
    
    (RAm<100 -> (tvalid([B],C,A,LBC,LAB), tvalid([C],B,A,LBC,LAC), LABC=[d([B,C],A) | LABC1], RA=RAm)
     ; ((PA=<2, tvalid([B],C,A,LBC,LAB), tvalid([C],B,A,LBC,LAC), LABC=[d([B,C],A) | LABC1], RA=2); (LABC=LABC1, RA=100))),
    (RBm<100 -> (tvalid([A],C,B,LAC,LAB), tvalid([C],A,B,LAC,LBC), LABC1=[d([A,C],B) | LABC2], RB=RBm)
     ; ((PB=<2, tvalid([A],C,B,LAC,LAB), tvalid([C],A,B,LAC,LBC), LABC1=[d([A,C],B) | LABC2], RB=2); (LABC1=LABC2, RB=100))),
    (RCm<100 -> (tvalid([A],B,C,LAB,LAC), tvalid([B],A,C,LAB,LBC), LABC2=[d([A,B],C)], RC=RCm)
     ; ((PC=<2, tvalid([A],B,C,LAB,LAC), tvalid([B],A,C,LAB,LBC), LABC2=[d([A,B],C)], RC=2); (LABC2=[], RC=100))).

case3f([FL2,FL1],A,B,C,RA,RB,RC) :-
    case3(FL2,LAB,LAC,LBC,A,B,C,RA,RB,RC,RA,RB,RC),
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

case3fr(RFL2,RFL1,A,B,C,RA,RB,RC) :- 
    cases5fr(RSs,_,A,B,C,RA,RB,RC), 
    member([RFL2,RFL1],RSs).

case4(LABCD,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
      A,B,C,D,RA,RB,RC,RD,PA,PB,PC,PD) :- 
    case3(LABC,LAB,LAC,LBC,A,B,C,RA1,RB1,RC1,PA,PB,PC), 
    case3(LABD,LAB,LAD,LBD,A,B,D,RA2,RB2,RD1,PA,PB,PD),
    case3(LACD,LAC,LAD,LCD,A,C,D,RA3,RC2,RD2,PA,PC,PD), 
    case3(LBCD,LBC,LBD,LCD,B,C,D,RB3,RC3,RD3,PB,PC,PD),

    min(RA1,RA2,RA3,RAm), min(RB1,RB2,RB3,RBm),
    min(RC1,RC2,RC3,RCm), min(RD1,RD2,RD3,RDm),

    (RAm<100 -> (tvalid([B,C],D,A,LBCD,LABC), tvalid([B,D],C,A,LBCD,LABD), tvalid([C,D],B,A,LBCD,LACD), 
		 LABCD=[d([B,C,D],A) | LABCD1], RA=RAm)
     ; ((PA=<3, tvalid([B,C],D,A,LBCD,LABC), tvalid([B,D],C,A,LBCD,LABD), tvalid([C,D],B,A,LBCD,LACD), 
	 LABCD=[d([B,C,D],A) | LABCD1], RA=3); 
	(LABCD=LABCD1, RA=100))),
    (RBm<100 -> (tvalid([A,C],D,B,LACD,LABC), tvalid([A,D],C,B,LACD,LABD), tvalid([C,D],A,B,LACD,LBCD), 
		 LABCD1=[d([A,C,D],B) | LABCD2], RB=RBm)
     ; ((PB=<3, tvalid([A,C],D,B,LACD,LABC), tvalid([A,D],C,B,LACD,LABD), tvalid([C,D],A,B,LACD,LBCD), 
	 LABCD1=[d([A,C,D],B) | LABCD2], RB=3); 
	(LABCD1=LABCD2, RB=100))),
    (RCm<100 -> (tvalid([A,B],D,C,LABD,LABC), tvalid([A,D],B,C,LABD,LACD), tvalid([B,D],A,C,LABD,LBCD), 
		 LABCD2=[d([A,B,D],C) | LABCD3], RC=RCm)
     ; ((PC=<3, tvalid([A,B],D,C,LABD,LABC), tvalid([A,D],B,C,LABD,LACD), tvalid([B,D],A,C,LABD,LBCD), 
	 LABCD2=[d([A,B,D],C) | LABCD3], RC=3); 
	(LABCD2=LABCD3, RC=100))),
    (RDm<100 -> (tvalid([A,B],C,D,LABC,LABD), tvalid([A,C],B,D,LABC,LACD), tvalid([B,C],A,D,LABC,LBCD), 
		 LABCD3=[d([A,B,C],D)], RD=RDm)
     ; ((PD=<3, tvalid([A,B],C,D,LABC,LABD), tvalid([A,C],B,D,LABC,LACD), tvalid([B,C],A,D,LABC,LBCD), 
	 LABCD3=[d([A,B,C],D)], RD=3); 
	(LABCD3=[], RD=100))).

case4f([FL3,FL2,FL1],A,B,C,D,RA,RB,RC,RD) :-
    case4(FL3,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
	  A,B,C,D,RA,RB,RC,RD,RA,RB,RC,RD),
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

case4fr(RFL3,RFL2,RFL1,A,B,C,D,RA,RB,RC,RD) :- 
    cases4fr(RSs,_,A,B,C,D,RA,RB,RC,RD), 
    member([RFL3,RFL2,RFL1],RSs).


case5(LABCDE,LABCD,LABCE,LABDE,LACDE,LBCDE,
      LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
      LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
      A,B,C,D,E,RA,RB,RC,RD,RE,PA,PB,PC,PD,PE) :- 
    case4(LABCD,LABC,LABD,LACD,LBCD,LAB,LAC,LAD,LBC,LBD,LCD,
	  A,B,C,D,RA1,RB1,RC1,RD1,PA,PB,PC,PD),
    case4(LABCE,LABC,LABE,LACE,LBCE,LAB,LAC,LAE,LBC,LBE,LCE,
	  A,B,C,E,RA2,RB2,RC2,RE1,PA,PB,PC,PE),
    case4(LABDE,LABD,LABE,LADE,LBDE,LAB,LAD,LAE,LBD,LBE,LDE,
	  A,B,D,E,RA3,RB3,RD2,RE2,PA,PB,PD,PE),
    case4(LACDE,LACD,LACE,LADE,LCDE,LAC,LAD,LAE,LCD,LCE,LDE,
	  A,C,D,E,RA4,RC3,RD3,RE3,PA,PC,PD,PE),
    case4(LBCDE,LBCD,LBCE,LBDE,LCDE,LBC,LBD,LBE,LCD,LCE,LDE,
	  B,C,D,E,RB4,RC4,RD4,RE4,PB,PC,PD,PE),

    min(RA1,RA2,RA3,RA4,RAm), min(RB1,RB2,RB3,RB4,RBm),
    min(RC1,RC2,RC3,RC4,RCm), min(RD1,RD2,RD3,RD4,RDm),
    min(RE1,RE2,RE3,RE4,REm),

    (RAm<100 -> (tvalid([B,C,D],E,A,LBCDE,LABCD), tvalid([B,C,E],D,A,LBCDE,LABCE), 
		 tvalid([B,D,E],C,A,LBCDE,LABDE), tvalid([C,D,E],B,A,LBCDE,LACDE),
		 LABCDE=[d([B,C,D,E],A) | LABCDE1], RA=RAm)
     ;((PA=<4, tvalid([B,C,D],E,A,LBCDE,LABCD), tvalid([B,C,E],D,A,LBCDE,LABCE), 
	       tvalid([B,D,E],C,A,LBCDE,LABDE), tvalid([C,D,E],B,A,LBCDE,LACDE),
	       LABCDE=[d([B,C,D,E],A) | LABCDE1], RA=4); 
       (LABCDE=LABCDE1, RA=100))),
    (RBm<100 -> (tvalid([A,C,D],E,B,LACDE,LABCD), tvalid([A,C,E],D,B,LACDE,LABCE), 
		 tvalid([A,D,E],C,B,LACDE,LABDE), tvalid([C,D,E],A,B,LACDE,LBCDE),
		 LABCDE1=[d([A,C,D,E],B) | LABCDE2], RB=RBm)
     ;((PB=<4, tvalid([A,C,D],E,B,LACDE,LABCD), tvalid([A,C,E],D,B,LACDE,LABCE), 
	       tvalid([A,D,E],C,B,LACDE,LABDE), tvalid([C,D,E],A,B,LACDE,LBCDE),
	       LABCDE1=[d([A,C,D,E],B) | LABCDE2], RB=4); 
       (LABCDE1=LABCDE2, RB=100))),
    (RCm<100 -> (tvalid([A,B,D],E,C,LABDE,LABCD), tvalid([A,B,E],D,C,LABDE,LABCE), 
		 tvalid([A,D,E],B,C,LABDE,LACDE), tvalid([B,D,E],A,C,LABDE,LBCDE),
		 LABCDE2=[d([A,B,D,E],C) | LABCDE3], RC=RCm)
     ;((PC=<4, tvalid([A,B,D],E,C,LABDE,LABCD), tvalid([A,B,E],D,C,LABDE,LABCE), 
	       tvalid([A,D,E],B,C,LABDE,LACDE), tvalid([B,D,E],A,C,LABDE,LBCDE),
	       LABCDE2=[d([A,B,D,E],C) | LABCDE3], RC=4); 
       (LABCDE2=LABCDE3, RC=100))),
    (RDm<100 -> (tvalid([A,B,C],E,D,LABCE,LABCD), tvalid([A,B,E],C,D,LABCE,LABDE), 
		 tvalid([A,C,E],B,D,LABCE,LACDE), tvalid([B,C,E],A,D,LABCE,LBCDE),
		 LABCDE3=[d([A,B,C,E],D) | LABCDE4], RD=RDm)
     ;((PD=<4, tvalid([A,B,C],E,D,LABCE,LABCD), tvalid([A,B,E],C,D,LABCE,LABDE), 
	       tvalid([A,C,E],B,D,LABCE,LACDE), tvalid([B,C,E],A,D,LABCE,LBCDE),
	       LABCDE3=[d([A,B,C,E],D) | LABCDE4], RD=4); 
       (LABCDE3=LABCDE4, RD=100))),
    (REm<100 -> (tvalid([A,B,C],D,E,LABCD,LABCE), tvalid([A,B,D],C,E,LABCD,LABDE), 
		 tvalid([A,C,D],B,E,LABCD,LACDE), tvalid([B,C,D],A,E,LABCD,LBCDE),
		 LABCDE4=[d([A,B,C,D],E)], RE=REm)
     ;((PE=<4, tvalid([A,B,C],D,E,LABCD,LABCE), tvalid([A,B,D],C,E,LABCD,LABDE), 
	       tvalid([A,C,D],B,E,LABCD,LACDE), tvalid([B,C,D],A,E,LABCD,LBCDE),
	       LABCDE4=[d([A,B,C,D],E)], RE=4); 
       (LABCDE4=[], RE=100))).

case5f([FL4,FL3,FL2,FL1],A,B,C,D,E,RA,RB,RC,RD,RE) :-
    case5(FL4,LABCD,LABCE,LABDE,LACDE,LBCDE,
      LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
      LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
	  A,B,C,D,E,RA,RB,RC,RD,RE,RA,RB,RC,RD,RE),
    append(LABCD,LABCE,LABDE,LACDE,LBCDE,FL3), 
    append(LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,FL2), 
    append(LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,FL1).

cases5f(Ss,N,A,B,C,D,E,RA,RB,RC,RD,RE) :- 
    findall(S,case5f(S,A,B,C,D,E,RA,RB,RC,RD,RE),Ss),length(Ss,N).

reducecases5f([],[],_,_,_,_,_).
reducecases5f([S | Ss],RSs,V,W,X,Y,Z) :- 
    u(PermVWXYZ,V,W,X,Y,Z), 
    replace(S,SP,[V,W,X,Y,Z],PermVWXYZ), 
    member(S2,Ss), similarcasef(SP,S2), !,
    reducecases5f(Ss,RSs,V,W,X,Y,Z).
reducecases5f([S | Ss],[S | RSs],V,W,X,Y,Z) :- reducecases5f(Ss,RSs,V,W,X,Y,Z).

cases5fr(RSs,RN,A,B,C,D,E,RA,RB,RC,RD,RE) :-
    cases5f(Ss,_,A,B,C,D,E,RA,RB,RC,RD,RE), reducecases5f(Ss,RSs,A,B,C,D,E), 
    length(RSs,RN).

case5fr(RFL4,RFL3,RFL2,RFL1,A,B,C,D,E,RA,RB,RC,RD,RE) :- 
    cases5fr(RSs,_,A,B,C,D,E,RA,RB,RC,RD,RE), 
    member([RFL4,RFL3,RFL2,RFL1],RSs).

case6(LABCDEF, LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,
	  LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
      A,B,C,D,E,F,RA,RB,RC,RD,RE,RF,PA,PB,PC,PD,PE,PF) :- 
    case5(LABCDE, LABCD,LABCE,LABDE,LACDE,LBCDE,
	  LABC,LABD,LABE,LACD,LACE,LADE,LBCD,LBCE,LBDE,LCDE,
	  LAB,LAC,LAD,LAE,LBC,LBD,LBE,LCD,LCE,LDE,
	  A,B,C,D,E,RA1,RB1,RC1,RD1,RE1,PA,PB,PC,PD,PE),
    case5(LABCDF, LABCD,LABCF,LABDF,LACDF,LBCDF,
	  LABC,LABD,LABF,LACD,LACF,LADF,LBCD,LBCF,LBDF,LCDF,
	  LAB,LAC,LAD,LAF,LBC,LBD,LBF,LCD,LCF,LDF,
	  A,B,C,D,F,RA2,RB2,RC2,RD2,RF1,PA,PB,PC,PD,PF),
    case5(LABCEF,LABCE,LABCF,LABEF,LACEF,LBCEF,
	  LABC,LABE,LABF,LACE,LACF,LAEF,LBCE,LBCF,LBEF,LCEF,
	  LAB,LAC,LAE,LAF,LBC,LBE,LBF,LCE,LCF,LEF,
	  A,B,C,E,F,RA3,RB3,RC3,RE2,RF2,PA,PB,PC,PE,PF),
    case5(LABDEF,LABDE,LABDF,LABEF,LADEF,LBDEF,
	  LABD,LABE,LABF,LADE,LADF,LAEF,LBDE,LBDF,LBEF,LDEF,
	  LAB,LAD,LAE,LAF,LBD,LBE,LBF,LDE,LDF,LEF,
	  A,B,D,E,F,RA4,RB4,RD3,RE3,RF3,PA,PB,PD,PE,PF),
    case5(LACDEF,LACDE,LACDF,LACEF,LADEF,LCDEF,
	  LACD,LACE,LACF,LADE,LADF,LAEF,LCDE,LCDF,LCEF,LDEF,
	  LAC,LAD,LAE,LAF,LCD,LCE,LCF,LDE,LDF,LEF,
	  A,C,D,E,F,RA5,RC4,RD4,RE4,RF4,PA,PC,PD,PE,PF),
    case5(LBCDEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
	  B,C,D,E,F,RB5,RC5,RD5,RE5,RF5,PB,PC,PD,PE,PF),


    min(RA1,RA2,RA3,RA4,RA5,RAm), min(RB1,RB2,RB3,RB4,RB5,RBm),
    min(RC1,RC2,RC3,RC4,RC5,RCm), min(RD1,RD2,RD3,RD4,RD5,RDm),
    min(RE1,RE2,RE3,RE4,RE5,REm), min(RF1,RF2,RF3,RF4,RF5,RFm),

    (RAm<100 -> (tvalid([B,C,D,E],F,A,LBCDEF,LABCDE), 
		 tvalid([B,C,D,F],E,A,LBCDEF,LABCDF), tvalid([B,C,E,F],D,A,LBCDEF,LABCEF), 
		 tvalid([B,D,E,F],C,A,LBCDEF,LABDEF), tvalid([C,D,E,F],B,A,LBCDEF,LACDEF),
		 LABCDEF=[d([B,C,D,E,F],A) | LABCDEF1], RA=RAm)
     ;((PA=<5, tvalid([B,C,D,E],F,A,LBCDEF,LABCDE), 
	       tvalid([B,C,D,F],E,A,LBCDEF,LABCDF), tvalid([B,C,E,F],D,A,LBCDEF,LABCEF), 
	       tvalid([B,D,E,F],C,A,LBCDEF,LABDEF), tvalid([C,D,E,F],B,A,LBCDEF,LACDEF),
	       LABCDEF=[d([B,C,D,E,F],A) | LABCDEF1], RA=5); 
       (LABCDEF=LABCDEF1, RA=100))),
    (RBm<100 -> (tvalid([A,C,D,E],F,B,LACDEF,LABCDE),
		 tvalid([A,C,D,F],E,B,LACDEF,LABCDF), tvalid([A,C,E,F],D,B,LACDEF,LABCEF), 
		 tvalid([A,D,E,F],C,B,LACDEF,LABDEF), tvalid([C,D,E,F],A,B,LACDEF,LBCDEF),
		 LABCDEF1=[d([A,C,D,E,F],B) | LABCDEF2], RB=RBm)
     ;((PB=<5, tvalid([A,C,D,E],F,B,LACDEF,LABCDE),
	       tvalid([A,C,D,F],E,B,LACDEF,LABCDF), tvalid([A,C,E,F],D,B,LACDEF,LABCEF), 
	       tvalid([A,D,E,F],C,B,LACDEF,LABDEF), tvalid([C,D,E,F],A,B,LACDEF,LBCDEF),
	       LABCDEF1=[d([A,C,D,E,F],B) | LABCDEF2], RB=5); 
       (LABCDEF1=LABCDEF2, RB=100))),
    (RCm<100 -> (tvalid([A,B,D,E],F,C,LABDEF,LABCDE),
		 tvalid([A,B,D,F],E,C,LABDEF,LABCDF), tvalid([A,B,E,F],D,C,LABDEF,LABCEF), 
		 tvalid([A,D,E,F],B,C,LABDEF,LACDEF), tvalid([B,D,E,F],A,C,LABDEF,LBCDEF),
		 LABCDEF2=[d([A,B,D,E,F],C) | LABCDEF3], RC=RCm)
     ;((PC=<5, tvalid([A,B,D,E],F,C,LABDEF,LABCDE),
	       tvalid([A,B,D,F],E,C,LABDEF,LABCDF), tvalid([A,B,E,F],D,C,LABDEF,LABCEF), 
	       tvalid([A,D,E,F],B,C,LABDEF,LACDEF), tvalid([B,D,E,F],A,C,LABDEF,LBCDEF),
	       LABCDEF2=[d([A,B,D,E,F],C) | LABCDEF3], RC=5); 
       (LABCDEF2=LABCDEF3, RC=100))),
    (RDm<100 -> (tvalid([A,B,C,E],F,D,LABCEF,LABCDE), 
		 tvalid([A,B,C,F],E,D,LABCEF,LABCDF), tvalid([A,B,E,F],C,D,LABCEF,LABDEF), 
		 tvalid([A,C,E,F],B,D,LABCEF,LACDEF), tvalid([B,C,E,F],A,D,LABCEF,LBCDEF),
		 LABCDEF3=[d([A,B,C,E,F],D) | LABCDEF4], RD=RDm)
     ;((PD=<5, tvalid([A,B,C,E],F,D,LABCEF,LABCDE), 
	       tvalid([A,B,C,F],E,D,LABCEF,LABCDF), tvalid([A,B,E,F],C,D,LABCEF,LABDEF), 
	       tvalid([A,C,E,F],B,D,LABCEF,LACDEF), tvalid([B,C,E,F],A,D,LABCEF,LBCDEF),
	       LABCDEF3=[d([A,B,C,E,F],D) | LABCDEF4], RD=5); 
       (LABCDEF3=LABCDEF4, RD=100))),
    (REm<100 -> (tvalid([A,B,C,D],F,E,LABCDF,LABCDE), 
		 tvalid([A,B,C,F],D,E,LABCDF,LABCEF), tvalid([A,B,D,F],C,E,LABCDF,LABDEF), 
		 tvalid([A,C,D,F],B,E,LABCDF,LACDEF), tvalid([B,C,D,F],A,E,LABCDF,LBCDEF),
		 LABCDEF4=[d([A,B,C,D,F],E) | LABCDEF5], RE=REm)
     ;((PE=<5, tvalid([A,B,C,D],F,E,LABCDF,LABCDE), 
	       tvalid([A,B,C,F],D,E,LABCDF,LABCEF), tvalid([A,B,D,F],C,E,LABCDF,LABDEF), 
	       tvalid([A,C,D,F],B,E,LABCDF,LACDEF), tvalid([B,C,D,F],A,E,LABCDF,LBCDEF),
	       LABCDEF4=[d([A,B,C,D,F],E) | LABCDEF5], RE=5); 
       (LABCDEF4=LABCDEF5, RE=100))),
    (RFm<100 -> (tvalid([A,B,C,D],E,F,LABCDE,LABCDF), 
		 tvalid([A,B,C,E],D,F,LABCDE,LABCEF), tvalid([A,B,D,E],C,F,LABCDE,LABDEF), 
		 tvalid([A,C,D,E],B,F,LABCDE,LACDEF), tvalid([B,C,D,E],A,F,LABCDE,LBCDEF),
		 LABCDEF5=[d([A,B,C,D,E],F)], RF=RFm)
     ;((PF=<5, tvalid([A,B,C,D],E,F,LABCDE,LABCDF), 
	       tvalid([A,B,C,E],D,F,LABCDE,LABCEF), tvalid([A,B,D,E],C,F,LABCDE,LABDEF), 
	       tvalid([A,C,D,E],B,F,LABCDE,LACDEF), tvalid([B,C,D,E],A,F,LABCDE,LBCDEF),
	       LABCDEF5=[d([A,B,C,D,E],F)], RF=5); 
       (LABCDEF5=[], RF=100))).

case6f([FL5,FL4,FL3,FL2,FL1],A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :-
    case6(FL5, LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,
	  LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,
	  LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,
	  LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,
	  A,B,C,D,E,F,RA,RB,RC,RD,RE,RF,RA,RB,RC,RD,RE,RF),
    append(LABCDE,LABCDF,LABCEF,LABDEF,LACDEF,LBCDEF,FL4),
    append(LABCD,LABCE,LABCF,LABDE,LABDF,LABEF,LACDE,LACDF,LACEF,LADEF,LBCDE,LBCDF,LBCEF,LBDEF,LCDEF,FL3), 
    append(LABC,LABD,LABE,LABF,LACD,LACE,LACF,LADE,LADF,LAEF,LBCD,LBCE,LBCF,LBDE,LBDF,LBEF,LCDE,LCDF,LCEF,LDEF,FL2), 
    append(LAB,LAC,LAD,LAE,LAF,LBC,LBD,LBE,LBF,LCD,LCE,LCF,LDE,LDF,LEF,FL1).

cases6f(Ss,N,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :- 
    findall(S,case6f(S,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF),Ss),length(Ss,N).

reducecases6f([],[],_,_,_,_,_,_).
reducecases6f([S | Ss],RSs,U,V,W,X,Y,Z) :- 
    u(PermUVWXYZ,U,V,W,X,Y,Z), 
    replace(S,SP,[U,V,W,X,Y,Z],PermUVWXYZ), 
    member(S2,Ss), similarcasef(SP,S2), !,
    reducecases6f(Ss,RSs,U,V,W,X,Y,Z).
reducecases6f([S | Ss],[S | RSs],U,V,W,X,Y,Z) :- reducecases6f(Ss,RSs,U,V,W,X,Y,Z).

cases6fr(RSs,RN,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :-
    cases6f(Ss,_,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF), reducecases6f(Ss,RSs,A,B,C,D,E,F), 
    length(RSs,RN).

case6fr(RFL5,RFL4,RFL3,RFL2,RFL1,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF) :- 
    cases6fr(RSs,_,A,B,C,D,E,F,RA,RB,RC,RD,RE,RF), 
    member([RFL5,RFL4,RFL3,RFL2,RFL1],RSs).
