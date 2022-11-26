
member(X,[X | _]).
member(X,[_ | Ys]) :- member(X,Ys).

memberdep(d([X],A),[d([X],A) | _]).
memberdep(d([X,Y],A),[d([U,V],A) | _]) :- u([U,V],X,Y).
memberdep(d([X,Y,Z],A),[d([U,V,W],A) | _]) :- u([U,V,W],X,Y,Z).
memberdep(d([W,X,Y,Z],A),[d([S,T,U,V],A) | _]) :- u([S,T,U,V],W,X,Y,Z).
memberdep(D,[_ | Ls]) :- memberdep(D,Ls).

memberdepexcl(d([X],A),[d([X],A) | Ls],Ls) :- !.
memberdepexcl(d([X,Y],A),[d([U,V],A) | Ls],Ls) :- u([U,V],X,Y), !.
memberdepexcl(d([X,Y,Z],A),[d([U,V,W],A) | Ls],Ls) :- u([U,V,W],X,Y,Z), !.
memberdepexcl(d([W,X,Y,Z],A),[d([S,T,U,V],A) | Ls],Ls) :- 
    u([S,T,U,V],W,X,Y,Z), !.
memberdepexcl(D,[L1 | Ls],[L1 | LsE]) :- memberdepexcl(D,Ls,LsE).

similardeps([],[]).
similardeps([D | L1s],L2) :- memberdepexcl(D,L2,L2s), similardeps(L1s,L2s).

similarcasef([],[]).
similarcasef([L | Ls],[L2 | L2s]) :- length(L,LN), length(L2,LN), similardeps(L,L2), similarcasef(Ls,L2s).


append([],Ys,Ys).
append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).

append(Ws, Xs, Ys, Zs) :- append(Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Vs, Ws, Xs, Ys, Zs) :- append(Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ss, Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Rs, Ss, Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).
append(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Ys, Zs) :- 
    append(Ps, Qs, Rs, Ss, Ts, Us, Vs, Ws, Xs, Zs1), append(Zs1, Ys, Zs).

min(X,Y,M) :- X>Y, !, M=Y.
min(X,_,X).

min(X,Y,Z,M) :- min(X,Y,M1), min(M1,Z,M).
min(W,X,Y,Z,M) :- min(W,X,M1), min(Y,Z,M2), min(M1,M2,M).


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
    
%    \+t1(LABC,LAB,LAC,LBC).

%t1(LABC,LAB,LAC,LBC) :- append(LAB,LAC,LBC,LABACBC),
%    memberdep(d([X,Y],Z),LABC), 
%    memberdep(d([X],Y),LABACBC),
%    \+memberdep(d([X],Z),LABACBC).

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

%    \+t2(LABCD,LABC,LABD,LACD,LBCD).

%t2(LABCD,LABC,LABD,LACD,LBCD) :- 
%    append(LABC,LABD,LACD,LBCD,LABCABDACDBCD), 
%    memberdep(d([X,Y,Z],W),LABCD),
%    memberdep(d([X,Y],Z),LABCABDACDBCD),
%    \+memberdep(d([X,Y],W),LABCABDACDBCD).

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

%    \+t3(LABCDE,LABCD,LABCE,LABDE,LACDE,LBCDE).

%t3(LABCDE,LABCD,LABCE,LABDE,LACDE,LBCDE) :- 
%    append(LABCD,LABCE,LABDE,LACDE,LBCDE,L4), 
%    memberdep(d([W,X,Y,Z],V),LABCDE),
%    memberdep(d([X,Y,Z],W),L4),
%    \+memberdep(d([X,Y,Z],V),L4).

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
