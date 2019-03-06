% No modificar el archivo de animales una vez akinator.pl está cargado!
:- [animales].

% Punto de entrada del juego
jugar :-
    writeln('Jugar o salir?'),
    read(Respuesta),
    ( jugar(Respuesta) ->
        animales(Animales),
        hacer_preguntas(Animales, Candidatos, Tiene),
        evaluar_candidatos(Candidatos, Tiene),
        main
    ; salir(Respuesta) -> true
    ; main ).

% Lista de preguntas
% pregunta(Nombre, CondicionYaTiene, Mensaje).
preguntas([
    pregunta(huevos, huevos, 'Pone huevos?'),
    pregunta(cuerno, cuerno, 'Tiene algún cuerno?'),
    pregunta(existe, existe, 'Existe?'),
    pregunta(domesticado, domesticado, 'Esta domesticado?'),
    pregunta(negro, negro, 'Se puede ser negro?'),
    pregunta(comer, comer, 'En España se come?'),
    pregunta(olfato, olfato, 'Tiene buen olfato?'),
    pregunta(tamano(pequeno), tamano(_), 'Es pequeno?'),
    pregunta(tamano(grande), tamano(_), 'Es grande?'),
    pregunta(tipo(mamifero), tipo(_), 'Es mamifero?'),
    pregunta(tipo(pez), tipo(_), 'Es pez?'),
    pregunta(tipo(reptil), tipo(_), 'Es reptil?'),
    pregunta(tipo(anfibio), tipo(_), 'Es anfibio?'),
    pregunta(tipo(ave), tipo(_), 'Es ave?'),
    pregunta(tipo(insecto), tipo(_), 'Es insecto?'),
    pregunta(habitat(agua), habitat(_), 'Es de habitat acuatico?'),
    pregunta(habitat(tierra), habitat(_), 'Es de habitat terrestre?')
]).

% Acumula los animales a una lista.
animales(Animales) :- findall(animal(N, Ds), animal(N, Ds), Animales).

% Hace todas las preguntas y obtiene las conclusiones.
hacer_preguntas(Candidatos, Candidatos1, Tiene1) :-
    preguntas(Ps),
    random_permutation(Ps, Ps1),
    hacer_preguntas(Ps1, Candidatos, [], Candidatos1, Tiene1).

% Hace preguntas y obtiene conclusiones.
hacer_preguntas([], Candidatos, Tiene, Candidatos, Tiene).
hacer_preguntas([Pregunta|Preguntas], Candidatos, Tiene, Candidatos1, Tiene1) :-
    hacer_pregunta(Pregunta, Candidatos, Tiene, Cs, Ts),
    hacer_preguntas(Preguntas, Cs, Ts, Candidatos1, Tiene1).

% Realiza una pregunta y obtiene conclusiones.
hacer_pregunta(Pregunta, Candidatos, Tiene, Candidatos1, Tiene1) :-
    pregunta(Dato, CondicionYaTiene, Mensaje) = Pregunta,
    ( memberchk(CondicionYaTiene, Tiene) ->
        % Si ya conocemos un dato relacionado, omitimos la pregunta
        Candidatos1 = Candidatos, Tiene1 = Tiene
    ;
        % Si no conocemos un dato relacionado, preguntamos y filtramos los candidatos
        writeln(Mensaje),
        read(Respuesta),
        ( si(Respuesta) -> filtrar_tiene(Dato, Candidatos, Candidatos1), Tiene1 = [Dato|Tiene]
        ; no(Respuesta) -> filtrar_no_tiene(Dato, Candidatos, Candidatos1), Tiene1 = Tiene
        ; hacer_pregunta(Pregunta, Candidatos, Tiene, Candidatos1, Tiene1)
        )
    ).

% Filtra los candidatos que tienen cierto dato.
filtrar_tiene(_, [], []).
filtrar_tiene(Dato, [C|Cs], Candidatos1) :-
    C = animal(_, Datos),
    ( memberchk(Dato, Datos) -> Candidatos1 = [C|Cs1]
    ; Candidatos1 = Cs1
    ),
    filtrar_tiene(Dato, Cs, Cs1).

% Filtra los candidatos que no tienen cierto dato.
% Es identica a filtrar_tiene/2 con las ramas del condicional intercambiadas.
filtrar_no_tiene(_, [], []).
filtrar_no_tiene(Dato, [C|Cs], Candidatos1) :-
    C = animal(_, Datos),
    ( memberchk(Dato, Datos) -> Candidatos1 = Cs1
    ; Candidatos1 = [C|Cs1]
    ),
    filtrar_no_tiene(Dato, Cs, Cs1).

% Pregunta si alguno de los candidatos finales es correcto.
% Si no, crea uno nuevo.
evaluar_candidatos([], Tiene) :- nuevo_animal(Tiene).
evaluar_candidatos(Candidatos, Tiene) :-
    Candidatos = [animal(Nombre, _)|Cs],
    format('Tu animal es ~s?~n', [Nombre]),
    read(Respuesta),
    ( si(Respuesta) -> mostrar_arte(Nombre)
    ; no(Respuesta) -> evaluar_candidatos(Cs, Tiene)
    ; evaluar_candidatos(Candidatos, Tiene)
    ).

% Añade un animal a partir de sus datos conocidos.
nuevo_animal(Tiene) :-
    writeln('No reconozco tu animal, quieres anadirlo? '),
    read(Respuesta),
    ( si(Respuesta) ->
        % Preguntamos por el nombre del nuevo animal
        writeln('Como se llama?'),
        read(Nombre),
        ( atom(Nombre), \+ animal(Nombre, _) ->
            % Si es correcto y no esta en la lista, lo anadimos
            assertz(animal(Nombre, Tiene)),
            tell('animales.pl'),
            listing(animal/2),
            told
        ; nuevo_animal(Tiene)
        )
    ; no(Respuesta) -> true
    ; nuevo_animal(Tiene)
    ).

% Muestra, si existe, el arte ASCII de un animal.
mostrar_arte(Nombre) :-
    ruta_arte(Nombre, Ruta) ->
        open(Ruta, read, Stream),
        read_string(Stream, _, Arte),
        close(Stream),
        writeln(Arte)
    % Si no existe, lo ignoramos.
    ; true.

% Obtiene, si existe, la ruta al arte ASCII de un animal.
ruta_arte(Nombre, Ruta) :-
    directory_file_path('art', Nombre, Base),
    file_name_extension(Base, 'txt', Ruta),
    access_file(Ruta, read).

% Predicados para entrada de usuario
jugar(j).
jugar(jugar).
jugar(p).
jugar(play).

salir(s).
salir(salir).
salir(x).
salir(e).
salir(exit).
salir(q).
salir(quit).

si(s).
si(si).
si(y).
si(yes).

no(n).
no(no).
