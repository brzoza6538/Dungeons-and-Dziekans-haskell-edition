/* Interaction with caretaker */
interact_with(caretaker) :-
    write('Spotykasz dozorcę.'), nl, nl,
    write('<LORE>'), nl,
    write('Dostępne odpowiedzi:'), nl,
    write('1. Czy mógłbyś mnie po prostu wypuścić?'), nl,
    write('2. Tak? No to rozpocznijmy walkę!'), nl,
    write('3. Rozumiem, w takim razie po prostu wrócę.'), nl,
    get_user_choice(Choice),
    caretaker_interaction(Choice).

interact_with(lecturer) :-
    write('Spotykasz wykładowcę.'), nl, nl,
    write('<LORE>'), nl,
    write('Dostępne odpowiedzi:'), nl,
    write('1. Naprawdę, ja to wiem, czy musimy to robić?'), nl,
    write('2. Dobrze, udowodnię sowją wiedzę.'), nl,
    get_user_choice(Choice),
    lecturer_interaction(Choice).

interact_with(dean) :-
    write('Spotykasz dziekana.'), nl, nl,
    write('<LORE>'), nl,
    write('Dostępne odpowiedzi:'), nl,
    write('1. A nie moglibyśmy jakoś umorzyć tych warunków?'), nl,
    write('2. Dobrze, udowodnię swoją wiedzę'), nl,
    ( holding(zloty_strzal) ->
        write('3. Mam złoty strzał, a regulamin mówi jasno - wszystkie warunki zostają wtedy umorzone; prawda?'), nl
    ; true
    ),
    get_user_choice(Choice),
    dean_interaction(Choice).

get_user_choice(Choice) :-
    nl, write('Wybór: '),
    read(Choice).


chance_success(Requirement) :-
    Requirement >= 1,
    random_between(1, 2, Outcome),
    Outcome = 1.


caretaker_interaction(1) :-
    curr_charisma(Charisma),
    ( chance_success(Charisma) ->
        write('<LORE>'),
        write('Dozorca daje się przekonać. Możesz teraz wyjść z budynku.'), nl
        /* upewnienie się czy chcesz i final scene */
    ; 
        write('Dozorca nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(caretaker)
    ).

caretaker_interaction(2) :-
    write('Postanawiasz zaatakować dozorcę!'), nl,
    start_battle(caretaker).

caretaker_interaction(3) :-
    write('<LORE>').

caretaker_interaction(_) :-
    write('Niepoprawna opcja. Spróbuj ponownie.'), nl,
    get_user_choice(Choice),
    caretaker_interaction(Choice).


lecturer_interaction(1) :-
    /* random for success only when curr_charisma >= 1 */
    curr_charisma(Charisma),
    ( chance_success(Charisma) ->
        write('Wykładowca daje się przekonać. Możesz iść dalej.'), nl
    ; 
        write('Wykładowca nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(lecturer)
    ).

lecturer_interaction(2) :-
    write('Postanawiasz zaatakować wykładowcę!'), nl,
    start_battle(lecturer).

lecturer_interaction(_) :-
    write('Niepoprawna opcja. Spróbuj ponownie.'), nl,
    get_user_choice(Choice),
    lecturer_interaction(Choice).



dean_interaction(1) :-
    /* random for success only when curr_charisma >= 1 */
    curr_charisma(Charisma),
    ( chance_success(Charisma) ->
        write('Dziekan daje się przekonać. Możesz iść dalej.'), nl
    ; 
        write('Dziekan nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(dean)
    ).

dean_interaction(2) :-
    write('Postanawiasz zaatakować dziekana!'), nl,
    start_battle(dean).

dean_interaction(3) :-
    holding(zloty_strzal),
    write('<LORE>'), nl.

dean_interaction(_) :-
    write('Niepoprawna opcja. Spróbuj ponownie.'), nl,
    get_user_choice(Choice),
    dean_interaction(Choice).