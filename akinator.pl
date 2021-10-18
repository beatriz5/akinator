% No modificar el archivo de animales una vez akinator.pl está cargado!
%no cambios a animales en tiempo de ejec
:- [animales].

:- use_module(library(random), [random_permutation/2]).

% Punto de entrada del juego
jugar :-
    writeln('Jugar o salir?'),
    read(Respuesta),
    ( respuesta(jugar, Respuesta) ->    %respuesta(jugar, j).
        animales(Animales),
        hacer_preguntas(Animales, Candidatos, Tiene),
        evaluar_candidatos(Candidatos, Tiene),
        jugar
    ; respuesta(salir, Respuesta) -> true  %respuesta(salir, q).
    ; jugar ).

% Lista de preguntas
% pregunta(Nombre, CondicionYaTiene, Mensaje).
preguntas([
    %preguntas es un predicado que tienes 1 lista de terminos 
    %(un termino ej pregunta(huevos, huevos, 'Does it lay eggs?'),)
    pregunta(huevos, huevos, 'Does it lay eggs?'),
    pregunta(cuerno, cuerno, 'Any horns?'),
    pregunta(existe, existe, 'Does the animal exist?'),
    pregunta(domesticado, domesticado, 'Is it a domesticated animal?'),
    pregunta(negro, negro, 'Is the animal black?'),
    pregunta(comer, comer, 'Do people eat this animal?'),
    pregunta(olfato, olfato, 'Does it have a good sense of smell?'),
    pregunta(tamano(pequeno), tamano(_), 'Is it small?'),
    pregunta(tamano(grande), tamano(_), 'Is it big?'),
    pregunta(tipo(mamifero), tipo(_), 'Is it a mammal?'),
    pregunta(tipo(pez), tipo(_), 'IS it a fish?'),
    pregunta(tipo(reptil), tipo(_), 'Is it a reptile?'),
    pregunta(tipo(anfibio), tipo(_), 'Is it amphibian?'),
    pregunta(tipo(ave), tipo(_), 'Is it a bird?'),
    pregunta(tipo(insecto), tipo(_), 'Is it an insect?'),
    pregunta(habitat(agua), habitat(_), 'Is it aquatic?'),
    pregunta(habitat(tierra), habitat(_), 'Is it a terrestial animal?')
]).

% Acumula los animales a una lista.
animales(Animales) :- findall(animal(N, Ds), animal(N, Ds), Animales).

% Hace todas las preguntas y obtiene las conclusiones.
hacer_preguntas(Candidatos, Candidatos1, Tiene1) :-   %hacer_preguntas(Animales, Candidatos, Tiene),
    preguntas(Ps),
    random_permutation(Ps, Ps1),
    hacer_preguntas(Ps1, Candidatos, [], Candidatos1, Tiene1).

% Hace preguntas y obtiene conclusiones.   %los acabados en 1 son resultado final
hacer_preguntas([], Candidatos, Tiene, Candidatos, Tiene).
hacer_preguntas([Pregunta|Preguntas], Candidatos, Tiene, Candidatos1, Tiene1) :-
    hacer_pregunta(Pregunta, Candidatos, Tiene, Cs, Ts),
    hacer_preguntas(Preguntas, Cs, Ts, Candidatos1, Tiene1).

% Realiza una pregunta y obtiene conclusiones.
hacer_pregunta(Pregunta, Candidatos, Tiene, Candidatos1, Tiene1) :-
    %descompone pregunta a como era originalmente
    pregunta(Dato, CondicionYaTiene, Mensaje) = Pregunta, 
    ( memberchk(CondicionYaTiene, Tiene) ->
        % Si ya conocemos un dato relacionado, omitimos la pregunta.
        Candidatos1 = Candidatos, Tiene1 = Tiene
    ;
        % Si no conocemos un dato relacionado, preguntamos y filtramos los candidatos.
        writeln(Mensaje),
        read(Respuesta),       %respuesta(si, s). deja los que tiene el dato vs deja los que no lo tiene
        ( respuesta(si, Respuesta) -> filtrar_tiene(Dato, Candidatos, Candidatos1), Tiene1 = [Dato|Tiene]
        ; respuesta(no, Respuesta) -> filtrar_no_tiene(Dato, Candidatos, Candidatos1), Tiene1 = Tiene
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
filtrar_no_tiene(Dato, [C|Cs], Candidatos1) :-   %deja los que no lo tiene
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
    ( respuesta(si, Respuesta) -> mostrar_arte(Nombre)   %respuesta(si, y).
    ; respuesta(no, Respuesta) -> evaluar_candidatos(Cs, Tiene)
    ; evaluar_candidatos(Candidatos, Tiene)
    ).

% Añade un animal a partir de sus datos conocidos.
nuevo_animal(Tiene) :-
    writeln('No reconozco tu animal, quieres anadirlo? '),
    read(Respuesta),
    ( respuesta(si, Respuesta) ->
        % Preguntamos por el nombre del nuevo animal
        writeln('Como se llama?'),
        read(Nombre),
        ( atom(Nombre), \+ animal(Nombre, _) ->
            % Si es correcto y no esta en la lista, lo anadimos
            assertz(animal(Nombre, Tiene)), %añade nuevo predicado
            tell('animales.pl'),            %abre arch
            listing(animal/2),          %guarda arch
            told                        %cierra
        ; nuevo_animal(Tiene)
        )
    ; respuesta(no, Respuesta) -> true
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
respuesta(jugar, j).
respuesta(jugar, jugando).
respuesta(jugar, p).
respuesta(jugar, play).
respuesta(salir, s).
respuesta(salir, salir).
respuesta(salir, x).
respuesta(salir, e).
respuesta(salir, exit).
respuesta(salir, q).
respuesta(salir, quit).
respuesta(si, s).
respuesta(si, si).
respuesta(si, y).
respuesta(si, yes).
respuesta(no, n).
respuesta(no, no).
