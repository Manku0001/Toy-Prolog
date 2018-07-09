edge(a,b).
edge(b,c).
edge(c,a).
path(X,X):-!.
path(X,Y) :- edge(X,Z),path_(Z,Y).
cycle(X) :- path(X,X).
