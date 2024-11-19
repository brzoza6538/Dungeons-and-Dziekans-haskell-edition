:- dynamic i_am_at/1, obcjet_at/2, npc_at/2, holding/1, used/1.
:- retractall(object_at(_, _)), retractall(npc_at(_, _)), retractall(i_am_at(_)), retractall(used(_)).

% Loading map
:- ensure_loaded('./map.pl').
:- ensure_loaded('./stats.pl').
:- ensure_loaded('./items.pl').
:- ensure_loaded('./interactions.pl').
:- ensure_loaded('./battle.pl').
:- ensure_loaded('./endings.pl').
:- choose_random_locations.
:- game_start_boost.

i_am_at(start).

/* This rule just writes out game instructions. */
instructions :-
        nl,
        write('Wpisuj komendy zgodnie ze standardową składnią Prologa.'), nl,
        write('Dostępne akcje to:'), nl,
        write('start.             -- Rozpocznij grę.'), nl,
        write('n.                 -- Idź w kierunku północnym.'), nl,
        write('e.                 -- Idź w kierunku wschodnim.'), nl,
        write('w.                 -- Idź w kierunku zachodnim.'), nl,
        write('s.                 -- Idź w kierunku południowym.'), nl,
        write('up.                -- Idź na górę.'), nl,
        write('down.              -- Idź na dół.'), nl,
        write('take(id).          -- Podnieś przedmiot.'), nl,
        write('look.              -- Rozejrzyj się.'), nl,
        write('stats.             -- Wyświetl swoje statystyki.'), nl,
        write('items.             -- Wyświetl swoje przedmioty.'), nl,
        write('instructions.      -- Wyświetl instrukcje ponownie.'), nl,
        write('halt.              -- Zakończ rozgrywkę i wyjdź.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */
start :-
		write("Witaj w Dungeons and Dziekans. Jesteś studentem, który niedawno obronił pracę inżynierską. Przy odbiorze dyplomu z dziekanatu wyszło na jaw, że nie opłaciłeś warunku. Znajdź dziekana i się z nim rozmów lub ucieknij niepostrzeżenie."),
        instructions,
        look.

/* Rules from example*/
/* These rules describe how to pick up an object. */
take(X) :-
        holding(X),
        write('Już to masz!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        object_at(X, Place),
        retract(object_at(X, Place)),
        assert(holding(X)),
        item(X, Name, _, _, _),
        format("Podniosłeś ~w.~n", [Name]),
        boost_stat(X),
        !, nl.

take(_) :-
        write('Nie ma tego tutaj.'),
        nl.


/* These rules describe how to put down an object. */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        item(X, Name, _, _, _),
        format("Upuściłeś ~w.~n", [Name]),
        decrease_stat(X),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).
up :- go(up).
down :- go(down).

/* This rule tells how to move in a given direction. */
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('Nie możesz tam iść.').

/* This rule tells how to look about you. */
look :-
        i_am_at(staircase_1),
        describe(staircase_1),
		write('Widzisz schody na pierwszym piętrze.'),
		nl,
		find_directions(staircase_1),
        nl.

look :-
        i_am_at(staircase_2),
        describe(staircase_2),
		write('Widzisz schody na drugim piętrze.'),
		nl,
		find_directions(staircase_2),
        nl.

look :-
        i_am_at(Place),
        describe(Place),
		find_directions(Place),
        nl,
        notice_objects_at(Place),
		notice_npcs_at(Place),
        nl.

/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
        object_at(X, Place),
        item(X, Name, _, _, _),
        write('Znajdujesz '), write(Name), write('.'), nl,
        write('Aby go wziąć, wpisz take('), write(X), write(').'), nl,
        fail.

notice_objects_at(_).

notice_npcs_at(Place) :-
        npc_at(NPC, Place),
        interact_with(NPC),
        fail.

notice_npcs_at(_).

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(Location) :-
        ( alt_info(Location, AltDescription) ->
                write(AltDescription), nl
        ;
                write('Idziesz korytarzem.'), nl
        ).

find_directions(Place) :-
		findall(Direction, path(Place, Direction, _), Directions),
		sort(Directions, UniqueDirections),
		write('Dostępne ruchy: '), write(UniqueDirections),
		nl.

/* Displaying items */
items :-
    findall(Item, holding(Item), Items),
    ( Items = [] ->
        write('Nie masz żadnych przedmiotów.'), nl
    ;
        write('Masz przy sobie:'), nl, nl,
        display_items(Items)
    ).

display_items([]).
display_items([Item | Rest]) :-
    item(Item, Name, Description, _, _),
    format('- ~w: ~w~n', [Name, Description]),
    display_items(Rest).