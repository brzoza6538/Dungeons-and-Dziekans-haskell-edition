/* Interaction with caretaker */
interact_with(caretaker) :-
    write('Zauważasz starszego mężczyznę stojącego przed wejściem. Czy zawsze tutaj stał?'), nl, nl, 
    /* tu tylko foreshadowing, historia dozorcy dopiero podczas rozmowy */
    write('Jego posiwiałe włosy i zmarszczki opowiadają historię o zmarnowanej młodości i życiu pełnego stresu i żalów. Jednak nie masz czasu na sympatię. Kiedy tylko cię zauważył wiedziałeś że nie przepuści cię doborwolnie' ), nl,
    write('Dostępne odpowiedzi:'), nl,
    write('1. Przepraszam, muszę przejść'), nl,
    write('2. *Zaczynasz przeglądać notatki, przygotowywując się na najgorsze*'), nl, 
    write('3. *powoli odsuwasz się z pola widzenia stróża*'), nl,
    get_user_choice(Choice),
    caretaker_interaction(Choice).

interact_with(lecturer) :-
    write('Przed tobą pojawia się dziki wykładowca. Nie jesteś pewnien jego nastawienia. Lepiej uważać'), nl, nl,
    write('Pragnie cię odpytać czy jedynie porozmawiać?'), nl,
    write('Dostępne odpowiedzi:'), nl,
    write('1. Pierwsze wrażenie jest najważniejsze. Dzień dobry panie magistrze doktorze doktorancki od spraw bardzo ważnych'), nl,
    write('2. Nie mogę ryzykować. Skończę tą odpytkę zanim nawet się zacznie '), nl,
	assert(npc_at(caretaker, hall1_5_i)),
    get_user_choice(Choice),
    lecturer_interaction(Choice).

interact_with(dean) :-
    write(' Zauważasz niewielki błysk na końcu korytarzam. Z zaciekawieniem postanawisz podejść bliżej. Jednak szybko żałujesz tej decyzji kiedy Z cienia wyłania się dziekan szczerząc się do ciebie   '), nl, nl,
    write('Twoje żałosne próby zdania semestru kosztowały mnie już wystarczająco dużo czasu. Twój czas dobiegł końca, mówi z sadystyczną satysfakcją '),
    write('Dostępne odpowiedzi:'), nl,
    write('1. Nie jestem już tym samym głupim studentem którego pan znał. Nie poddam się bez walki'), nl,
    write('2. Proszę nie, przysięgam że moje zaległe warunki to przeszłość'), nl,
    /* only available when user has zloty_strzal item */
    write('3. Niech pan patrzy co trzymam w ręce, to złoty strzał. Powołuję się na pradawne prawa tego świata, nie ma pan nade mną władzy'), nl,
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


chance_success(Requirement) :-
    Requirement >= 1,
    random_between(1, 2, Outcome),
    Outcome = 1.

chance_success2(Requirement) :-
    Requirement >= 1,
    random_between(1, 2, Outcome),
    Outcome = 1.


caretaker_interaction(1) :-
    curr_charisma(Charisma),
    ( chance_success(Charisma) ->
        write('"Hej, czy ty nie opłaciłeś warunku i chcesz teraz wyjść na pewniaka, bez konfrontacji z dziekanem?" pyta'), nl,
		write('Milczysz'), nl,
		write('"Wiesz," zaczyna powoli, "właściwie to przypominasz mi mnie samego z czasów studiów. Też walczyłem z systemem, dyskutowałem, kombinowałem, jak ominąć regulamin. Niestety nie udało się i zobacz, jak skończyłem. Nie chciałbym, żeby kogoś jeszcze to spotkało."'), nl,
        write('Dozorca ustąpił. Możesz teraz wyjść z budynku.'), nl,
		finish_fake
        /* upewnienie się czy chcesz i final scene */
    ; 
        write('Dozorca nie daje się przekonać. Rozpoczynasz walkę.'), nl,
        start_battle(caretaker)
    ).

caretaker_interaction(2) :-
    write('Postanawiasz zaatakować dozorcę!'), nl,
    start_battle(caretaker).

caretaker_interaction(3) :-
    write('"Hej! Co ty robisz tchórzu? Nie wypuszczę Cię! Przynieś kartkę od dziekana, że opłaciłeś warunek"'). 
	/*it was supposed to be lore here*/

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
    /* random for success only when curr_charisma >= 2 */
    curr_charisma(Charisma),
    ( chance_success2(Charisma) ->
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
    finish_peaceful. 
	/*it was supposed to be lore here*/

dean_interaction(_) :-
    write('Niepoprawna opcja. Spróbuj ponownie.'), nl,
    get_user_choice(Choice),
    dean_interaction(Choice).