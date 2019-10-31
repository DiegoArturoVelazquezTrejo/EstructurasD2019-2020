% todo hecho termina con un punto Los hechos implican las reglas <-
% En prolog todas las variables son mayúsculas
% Backtracking va regresando todos los valores posibles

persona(juan).

capital(usa, washington).
capital(uk, london).
capital(colombia, bogota).
capital(japon, tokio).
capital(usa, ny).
capital(mexico, ciudaddemexico).
capital(cabo_verde, praia).

paises(X,Y) :- persona(X), ingles(Y).
viajar(X,Z) :- paises(X,Y), capital(Y,Z).

moneda(uk, euros).
moneda(colombia, pesos).
moneda(mexico, pesos).
moneda(usa, dolar).
moneda(japon, yen).

lengua(usa, ingles).
lengua(uk, ingles).
lengua(mexico, espagnol).
lengua(colombia, espagnol).
lengua(japon, japones).

quevisitar(C,L) :- lengua(C,L).
quevisitar2(C,L,M) :- quevisitar(C,L) , moneda(A,L).

%-------------------------
%     Árbol genalógico
%         SIMPSON
%-------------------------
% Esto es un hecho
papa(homero, maggie).
papa(homero, lisa).
papa(homero, bart).
papa(abraham, homero).
papa(abraham, herbert).
papa(abraham, abbie).
papa(clancy, marge).
papa(clancy, selma).
papa(clancy, patty).
mama(marge, maggie).
mama(marge, lisa).
mama(marge, bart).
mama(mona, homero).
mama(edwina, abbie).
mama(???,herbert).
mama(jacqueline, marge).
mama(jacqueline, selma).
mama(jacqueline, patty).
mama(selma, ling).
%-------------------------


% Claramente Y es hijo de X si, X es papa de Y ó si X es mamá de Y.


%hss(A,B) :- aqui_va_su_código_:

primo(U,V) :- hijo(U, Y) , tio(Y, V), U\=Y.
hijo(Y,X) :- papa(X,Y); mama(X,Y).
tutor(A,B) :- hijo(B,A) ; tio(A, B) ; abuelo(A,B).
tio(S,T) :- padres(A,B,T),(hermano(S,A);hermano(S,B)), A\=B.
hermano(M,N) :- padres(A,B,M), padres(A,B,N), A\=B.
mediohermano(D,E) :- padres(A,B,M); padres(A,B,N), A\=B.
padres(A,B,C) :- hijo(C,A), hijo(C,B), A\=B.
abuelo(C,D) :- hijo(A, C) , hijo(D, A), C\=A.
nieto(E,F) :- abuelo(F, E).

%p([A|B]) :- aqui_va_su_código_:)
% ésto puede ser de ayuda =D print('→←')
