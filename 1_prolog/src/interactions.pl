/* Interaction with caretaker */
interact_with(caretaker) :-
    write('Zauważasz starszego mężczyznę stojącego przed wejściem. Czy zawsze tutaj stał?'), nl, nl, #tu tylko foreshadowing, historia dozorcy dopiero podczas rozmowy
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