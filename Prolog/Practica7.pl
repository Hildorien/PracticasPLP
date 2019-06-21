%Ejercicio 1
%I
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

%II
hijo(X,Y) :- padre(Y,X).
hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.
descendiente(X,Y) :- hijo(X,Y).
descendiente(X,Y) :- hijo(X,Z), descendiente(Z,Y).

%IV
%abuelo(juan,x).
%V 
%hermano(pablo,y).
%VI 
ancestro(X, X).
%ancestro(X, Y) :- ancestro(Z, Y) , padre(X, Z). ESTO NO TERMINA
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).

%Ejercicio 2
vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls). 

%Ejercicio 3
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

%Ejercicio 4
concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :- concatenar(L1, L2, L3).

%Ejercicio 5
%I
last([X],X).
last([_|XS],Y) :- last(XS,Y). 

%II
reverso([],[]).
reverso([X|XS],Y) :- reverso(XS,Z) , append(Z,[X],Y).

%III
%prefijo([],_).
%prefijo([X|XS], [X|L]) :- prefijo(XS,L). 
% a. prefijo(+L, ?P): que tiene éxito si P es un prefijo de la lista L.
prefijo(L, P) :- append(P, S, L).

% b. sufijo(+L, ?S): que tiene éxito si S es un sufijo de la lista L.
sufijo(L, S) :- append(P, S, L).

% c. sublista(+L, ?SL): que tiene éxito si SL es una sublista de L.
sublista(L, SL) :- prefijo(L, P), sufijo(P, SL).

%VI
pertenece(X,[X|_]).
pertenece(Y,[_|XS]) :- pertenece(Y,XS).

%Ejercicio 6
aplanar([],[]).
aplanar(X,[X]) :- not(is_list(X)). 
aplanar([X|XS], Y) :- aplanar(X,Z) , aplanar(XS,W) , concatenar(Z,W,Y).   

%Ejercicio 7. 
palindromo(X,Y) :- reverso(X,W), concatenar(X,W,Y).

iesimo(0,[X|_],X).
iesimo(I,[X|XS],Y) :- length([X|XS],W), between(0,W,I), N is I-1, iesimo(N,XS, Y).

%Ejercicio 8 
%I
interseccion([],_, []).
interseccion([X|XS],L,Y) :- not(member(X,L)) , interseccion(XS,L,Y).
interseccion([X|XS],L,Y) :- member(X,L) , interseccion(XS,L,Y) , member(X,Y).
interseccion([X|XS],L,Y) :- member(X,L),  interseccion(XS, L, W), not(member(X,W)) ,append([X], W, Y).
%II 
prefijoConLong(P,N,L) :- prefijo(L,P), length(P,N).
sufijoConLong(S,N,L) :- sufijo(L,S), length(S,N).

split(N,L,L1,L2) :- length(L,M), N1 is M-N, prefijoConLong(L1,N,L), sufijoConLong(L2,N1,L). 

%III 
%borrar(+L,+X,-XS) 
borrar([],_,[]).
borrar([X],X,[]).
borrar([X|XS], X, L ) :-  borrar(XS,X,L).
borrar([X|XS], Y, [X|Rec]) :- X\= Y, borrar(XS,Y,Rec).  

%IV sacarDuplicados(+L1,-L2)
sacarDuplicados([],[]).
sacarDuplicados([X|XS], [X | Rec]) :- borrar(XS,X,L), sacarDuplicados(L,Rec). 

%V
insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

permutaciones([X],[X]).
permutaciones([X|XS], P) :- permutaciones(XS,W), insertar(X,W,P).

%VI 
%reparto(+L,+N,-LL)
%sonTodasSublistas(+LL,+L)
sonTodasSublistas([],_).
sonTodasSublistas([X|XS], L) :- sublista(L,X) , sonTodasSublistas(XS,L).

concatenarTodas([],[]).
concatenarTodas([X|XS], LL) :- concatenarTodas(XS, Rec) , append(X,Rec,LL).

reparto(L,N,LL) :- length(LL,N) , sonTodasSublistas(LL,L) , concatenarTodas(LL,L). 


%V 
repartoSinVacias(L,LL) :- length(L,N), between(1,N,X), reparto(L,X,LL), not(member([],LL)).

%Ejercicio 9 . 
primeroDeTodas([],[]).
primeroDeTodas([[X|XS]|L], [X|YS]) :- primeroDeTodas(L,YS).

elementosTomadosEnOrden(_,0,[]).
elementosTomadosEnOrden(L,N,E) :- N =\= 0 , repartoSinVacias(L,LL) , length(LL,N) , primeroDeTodas(LL,E). 
elementosTomadosEnOrden([X|XS],N,E) :- length(XS,M), M >= N , elementosTomadosEnOrden(XS,N,E).
%Ejercicio 10. 
%desde(+X,-Y)
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).    

%desde2(+X,?Y) 
desde2(X,X).
desde2(X,Y) :- var(Y) , N is X+1 , desde(N,Y). 
desde2(X,Y) :- nonvar(Y) , Y > X.

%Ejercicio 11 
%intercalar(?X,?Y,?Z)
intercalar([],[],[]).
intercalar([],L,L).
intercalar(L,[],L).
intercalar([X|XS], [Y|YS], [X,Y| Rec]) :- intercalar(XS,YS,Rec).

%Ejercicio 12 
vacio(nil). 
raiz(bin(_,R,_),R). 
altura(nil, 0). 
altura(bin(I,_,D), N) :- altura(I,AI), altura(D,AD), N is max(AI,AD) +1. 
cantidadNodos(nil,0). 
cantidadNodos(bin(I,_,D), N) :- cantidadNodos(I,NI), cantidadNodos(D,ND), N is (NI + ND) + 1.

%Ejercicio 13 
%I inorder(+AB,-Lista)
inorder(nil, []).
inorder(bin(I,R,D),L) :- inorder(I,L1), inorder(D,L2), append(L1, [X| L2], L).

%II
%arbolConInorder(+L,-AB)
arbolConInorder([],nil).
arbolConInorder(L, bin(I,R,D)) :- 

%III 
esABB(nil). 
esABB(bin(I,R,D)) :- inorder(bin(I,R,D),L), sort(L,R).

%IV 
%aBBInsertar(+X,+T1,-T2)
%aBBInsertar(X,Nil,bin(Nil,X,Nil)).
%aBBInsertar(X,bin(I,R,D),bin(I2,R,D2)) :- X > Y , aBBInsertar(X, bin(I,R,D), bin(I,R,D2)). 
%aBBInsertar(X,bin(I,R,D),bin(I2,R,D2)) :- X <= Y , aBBInsertar(X, bin(I,R,D), bin(I2,R,D)). 

%Ejercicio 14 
armarPares(X,Y) :- desde(0,S), between(0,S,X), Y is S-X.
coprimos(X,Y) :- armarPares(X,Y), 1 =:= gcd(X,Y).

%Ejercicio 15 
listasQueSuman(0,[[]]).
listasQueSuman(N,L) :- between(1,N,X) , Y is N-X , listasQueSuman(Y,XS) , append(XS,W,L).   

%cuadradoSemiLatino(N,XS) :- between(0,N-1,X), listasQueSuman(X,L), length(L,N), append(L,XS).   

%Ejercicio 16
%I
esTriangulo(tri(A,B,C)) :- A < B+C, B < C+A , C < A+B.
%II
%perimetro(?T,?P)
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)) , esTriangulo(tri(A,B,C)) , P is A+B+C. %Caso de tri instanciado
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), desde2(3,X), between(1,X,A), S is X-A, between(1,S,B), C is S-B, esTriangulo(A,B,C). %Caso de tri no instanciado.

%Ejercicio 17 
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).
leGusta(X) :- frutal(X) , cremoso(X) .
cucurucho(X,Y) :- leGusta(X) , leGusta(Y) .

%Ejercicio 19
%Genero todos los cortes posibles en la lista, calculo sus sumas y almaceno sus diferencias
diferenciaCorte(L,L1,L2,D) :- append(L1,L2, L), sumlist(L1,S1) , sumlist(L2,S2), D is abs(S1-S2). 
%Busco la diferencia minima
hayMejores(L,D) :- diferenciaCorte(L,_,_,D2), D2 < D.
corteMasParejo(L,L1,L2) :- diferenciaCorte(L,L1,L2,D) , not(hayMejores(L,D)).


%Ejercicio 20 
diferenciaSimentrica([],L2,L2).
diferenciaSimentrica([X|XS], L2,L3) :- member(X,L2) , delete(L2,X,LL2) , diferenciaSimentrica(XS, LL2, L3). 
diferenciaSimentrica([X|XS], L2, [X|YS]) :- diferenciaSimentrica(XS, L2, YS). 
%diferenciaSimentrica(L1,L2,L3) :- subtract(L1,L2,LL1) , subtract(L2,L1,LL2) , union(LL1,LL2,L3).

%Ejercicio 22
pertenece(E,C).
%I
conjuntoDeNaturales([]).
conjuntoDeNaturales([X|XS]) :- natural(X) , conjuntoDeNaturales(XS).
%III
conjuntoDeNaturalesMalo(X) :- not( (not(natural(E)), pertenece(E,X)) ). % SOLO CHEQUEA LA PRIMER INSTANCIACION DE E.


%Ejercicio 24 
%I
arbol(nil). 
arbol(bin(I,R,D)) :- arbol(I) , arbol(D).
%II nodosEn(?A,+L)
nodosEn(nil,[]).
nodosEn(bin(nil,X,nil), [X]).
nodosEn(bin(I,R,D),[X|XS]) :- R = X , nodosEn(bin(I,R,D), XS) . 
nodosEn(bin(I,R,D),[X|XS]) :- R \= X , nodosEn(D, [X|XS]) , nodosEn(I, [X|XS]).
%III
