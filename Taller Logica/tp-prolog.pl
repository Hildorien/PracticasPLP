atributos(supernova, [mujer, extraterrestre, adulto, alto, inteligente, heroe]).
atributos(rick,[hombre, adulto, alto, inteligente, alcoholico, criminal]).
atributos(morty, [hombre, joven, bajo, torpe ]).
atributos(summer, [mujer, joven, alto, inteligente]). 
atributos(beth, [mujer, adulto, alto, inteligente, alcoholico ]). 
atributos(jerry, [hombre, adulto, alto, torpe]).
atributos(squanchy, [extraterrestre, criminal, inteligente, bajo ]).
atributos(mrPoopyButtHole, [extraterrestre, inteligente, bajo ]).
atributos(noobNoob, [extraterrestre, torpe, bajo, heroe]). 
atributos(birdPerson, [extraterrestre, inteligente, alto, criminal]).
atributos(evilMorty, [hombre, joven, bajo, inteligente, criminal]).
atributos(abradolfLincler, [hombre, adulto, inteligente, criminal, heroe]).
atributos(doofusRick, [hombre, adulto, alto, torpe]).

:- dynamic si/1, no/1, atributos/2.
 
adivinarPersonaje :- atributos(X, XS), satisfaceAtributos(XS), write('Tu personaje definitivamente es '), write(X), borraRespuestas, !.
adivinarPersonaje :- write('Mmm... para mi tu personaje es SEBI TABOH pero por las dudas, agregalo a nuestra base de datos'), write('\n'),crearPersonaje, borraRespuestas.

crearPersonaje :- write('Como es el nombre de tu personaje?\n'), read(NAME), write('Y los atributos?\n'), leerAtributos(NAME,[], 'si').

leerAtributos(NAME,ATTR, 'si') :- read(RATTR), write('Agregar otro?'),read(OTRO), leerAtributos(NAME, [RATTR|ATTR], OTRO).
leerAtributos(NAME,ATTR, 'no') :- agregarYaDefinidos([], ATTRDEF), append(ATTRDEF,ATTR,ATTRes), agregarPersonaje(NAME, ATTRes).

agregarYaDefinidos(BANNED, [X|L]) :- si(X), not(member(X,BANNED)), agregarYaDefinidos([X|BANNED],L), !.
agregarYaDefinidos(_, []).

evaluarSiTerminar(R) :- R == 'si',!.
evaluarSiTerminar(R) :- R == 'no', false.
%%evaluarSiTerminar(_) :- write('Respuesta inválida. Se pregunta nuevamente.\n'), pregunta(A).

% agregarPersonaje(+Nombre, +Atributos).
agregarPersonaje(NAME, ATTR) :- not(atributos(_,ATTR)),not(atributos(NAME,_)), assertz(atributos(NAME,ATTR)).

% mostrarPersonaje(+Nombre).
mostrarPersonaje(_) :- fail.

borraRespuestas :- retractall(si(_)), retractall(no(_)).

% atributos(?Nombre, ?Atributos).

% satisfaceAtributos(+Atributos).
satisfaceAtributos([]).
satisfaceAtributos([X|XS]) :- satisface(X), satisfaceAtributos(XS).

% satisface(+Atributo).
satisface(A) :- si(A), !.
satisface(A) :- not(no(A)), pregunta(A), si(A).

% pregunta(+Atributo).
pregunta(A) :- mostrarPregunta(A), leerRespuesta(R), guardarRespuesta(A,R).

% mostrarPregunta(+Atributo).
mostrarPregunta(A) :- write('Tu personaje es... \n\n'),write(A),nl,write('si/no?').

% leerRespuesta(-Respuesta).
leerRespuesta(R) :- read(R).

% guardarRespuesta(+Atributo, +Respuesta).
guardarRespuesta(A, R) :- R == 'si', !, assertz(si(A)).
guardarRespuesta(A, R) :- R == 'no', !, assertz(no(A)).
guardarRespuesta(A, _) :- write('Respuesta inválida. Se pregunta nuevamente.\n'), pregunta(A).


%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%
test(1) :- true.
tests :- forall(between(1,1,N), test(N)). % Hacer mejores tests y cambiar 1 por la cantidad de tests que tengan.

