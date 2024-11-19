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
    /* only available when user has zloty_strzal item */
    write('3. Mam złoty strzał, a regulamin mówi jasno - wszystkie warunki zostają wtedy umorzone; prawda?'), nl,
    get_user_choice(Choice),
    dean_interaction(Choice).

interact_with(automat) :-
	used(automat),
	write('Widzisz automat.'), nl, nl,
	write('Od zawsze stoi na wydziale, choć czasem zmienia miejsce. Zaopatrza studentów w produkty pierwszej potrzeby. Jesteś spłukany i nie możesz nic kupić.'), nl.

interact_with(automat) :-
	write('Widzisz automat.'), nl, nl,
    write('Od zawsze stoi na wydziale, choć czasem zmienia miejsce. Zaopatrza studentów w produkty pierwszej potrzeby. Masz puste kieszenie, ale zauważasz, że poprzedni klient musiał się bardzo śpieszyć i nie zdążył odebrać reszty... Możesz wybrać jedną z dostępnych pozycji.'), nl,
    write('Dostępne opcje:'), nl,
    write('1. Wybierz nowy regulamin'), nl,
    write('2. Wybierz lepsze notatki'), nl,
    write('3. Wybierz togę'), nl,
	write('4. Wybierz energetyka'), nl,
	write('5. Zrezygnuj z wyboru'), nl,
    get_user_choice(Choice),
    automat_interaction(Choice).

automat_interaction(1) :- 
	holding(nieaktualny_regulamin_studiow),
	write('Wymieniasz nieaktualny regulamin studiów na nowy.'), nl, nl,
	drop(nieaktualny_regulamin_studiow),
	assert(holding(nowy_regulamin)),
	format("Otrzymujesz ~w.~n", [nowy_regulamin]),
	boost_stat(nowy_regulamin),
	!, nl,
	assert(used(automat)).

automat_interaction(1) :- 
	assert(holding(nowy_regulamin)),
	format("Otrzymujesz ~w.~n", [nowy_regulamin]),
	boost_stat(nowy_regulamin),
	!, nl,
	assert(used(automat)).

automat_interaction(2) :- 
	holding(slabe_notatki),
	write('Wymieniasz słabe notatki na lepsze.'), nl, nl,
	drop(slabe_notatki),
	assert(holding(lepsze_notatki)),
	format("Otrzymujesz ~w.~n", [lepsze_notatki]),
	boost_stat(lepsze_notatki),
	!, nl,
	assert(used(automat)).

automat_interaction(2) :- 
	write('Otrzymujesz lepsze notatki.'), nl, nl,
	assert(holding(lepsze_notatki)),
	format("Otrzymujesz ~w.~n", [lepsze_notatki]),
	boost_stat(lepsze_notatki),
	!, nl,
	assert(used(automat)).

automat_interaction(3) :- 
	write('Otrzymujesz toge. Wygląda na to, że poprzedni student nie zdążył jej zabrać.'), nl,
	assert(holding(toga)),
	format("Otrzymujesz ~w.~n", [toga]),
	boost_stat(toga),
	!, nl,
	assert(used(automat)).

automat_interaction(4) :- 
	write('Otrzymujesz energetyka. Może przyda ci się na później.'), nl,
	assert(holding(energetyk)),
	format("Otrzymujesz ~w.~n", [energetyk]),
	boost_stat(energetyk),
	!, nl,
	assert(used(automat)).

automat_interaction(5) :- 
	write('Zachowujesz się uczciwie i rezygnujesz z zakupu za niewłasne pieniądze. Poprzedni klient wraca się, by odebrać resztę.'), nl,
	assert(used(automat)).

get_user_choice(Choice) :-
    nl, write('Wybór: '),
    read(Choice).



caretaker_interaction(1) :-
    /* random for success only when curr_charisma >= 1 */
    random_between(1, 2, Outcome),
    ( Outcome = 1 ->
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



lecturer_interaction(1) :-
    /* random for success only when curr_charisma >= 1 */
    random_between(1, 2, Outcome),
    ( Outcome = 1 ->
        write('Wykładowca daje się przekonać. Możesz iść dalej.'), nl
    ; 
        write('Wykładowca nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(lecturer)
    ).

lecturer_interaction(2) :-
    write('Postanawiasz zaatakować wykładowcę!'), nl,
    start_battle(lecturer).



dean_interaction(1) :-
    /* random for success only when curr_charisma >= 1 */
    random_between(1, 2, Outcome),
    ( Outcome = 1 ->
        write('Dziekan daje się przekonać. Możesz iść dalej.'), nl
    ; 
        write('Dziekan nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(dean)
    ).

dean_interaction(2) :-
    write('Postanawiasz zaatakować dziekana!'), nl,
    start_battle(dean).

/* only when zloty_strzal acquired */
dean_interaction(3) :-
    write('<LORE>'), nl.
    /* tu już chyba ending scene */