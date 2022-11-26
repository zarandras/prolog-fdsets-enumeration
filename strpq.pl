
strpq(L) :-
    buckets(L,LF),
    applyS(LF,LFS),
    applyT(LFS,LFST),
    applyR(LFST,LFSTR),
    applyPQ(LFSTR,LFSTRPQ),
    writeset(LFSTRPQ).

buckets([],s([],[])).
