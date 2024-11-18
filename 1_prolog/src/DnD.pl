:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(object_at(_, _)), retractall(npc_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

% Loading map
:- ensure_loaded('./map.pl').
:- choose_random_locations.

i_am_at(start).

/*Changed rules*/
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
        write('take(Object).      -- Podnieś przedmiot.'), nl,
        write('look.              -- Rozejrzyj się.'), nl,
        write('instructions.      -- Wyświetl instrukcje ponownie.'), nl,
        write('halt.              -- Zakończ rozgrywkę i wyjdź.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */
start :-
		write("Witaj w Dungeons and Dziekans. Jesteś studentem, który niedawno obronił pracę inżynierską. Przy odbiorze dyplomu z dziekanatu wyszło na jaw, że nie opłaciłeś warunku. Znajdź dziekana i się z nim rozmów lub ucieknij niepostrzeżenie."),
        instructions,
        look.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */
finish_true :-
        nl,
        write('Po godzinach gorącej debaty i zbijaniu kolejnych biurokratycznych absurdów, dziekan w końcu ustępuje.'),
		nl,
		write('"Masz rację," mówi z nutą zmęczenia, "ten system nie jest doskonały, a twoja sytuacja jest wyjątkowa. Zobowiązuję się do umorzenia twojego warunku i wykreślenia cię z listy dłużników." Czujesz, jak ogromny ciężar spada z twoich ramion. Opuszczasz gabinet z poczuciem zwycięstwa nad systemem, który miał cię zniszczyć, i wiarą w to, że można zmieniać rzeczywistość — choćby jedną debatą na raz.".'),
        nl,
		nl,
		write('Koniec rozgrywki. Wpisz "halt. by opuścić grę."'),
		nl.

finish_peaceful :-
        nl,
        write('Wręczasz złoty strzał dziekanowi. Przez chwilę patrzy na ciebie z niedowierzaniem, jakbyś znalazł coś, co istniało tylko w plotkach.'),
		nl,
		write('"Złoty strzał? Dawno tego nie widziałem... Masz szczęście, że nie usunęliśmy jeszcze tego przestarzałego zapisku z regulaminu. Ostateczna szansa, raz na całe życie. Długi anulowane, a ja nic nie widziałem. Tylko Nie mów nikomu, bo wszyscy zaczną mnie tym dręczyć. I nie licz na to, że drugi raz się uda."'),
		nl,
		nl,
		write('Koniec rozgrywki. Wpisz "halt. by opuścić grę."'),
		nl.

finish_lucky :-
        nl,
		nl,
		write('Koniec rozgrywki. Wpisz "halt. by opuścić grę."'),
		nl.

finish_fake :-
        nl,
        write('Udało Ci się ominąć stróża i wydostajesz się z budynku. Czujesz rosnącą ulgę z każdym krokiem oddalającym cię od biura dziekana. Na moment wydaje się, że wszystko jest w porządku - jakbyś naprawdę przechytrzył system. Twoje długi jednak nie zniknęły i nie będzie można przed nimi uciekać w nieskończoność. Odsetki od twojego długu wciąż rosną, a kolejne listy z uczelni zaczną spływać szybciej, niż zdążysz znaleźć kryjówkę, zmienić tożsamość lub znaleźć prawdziwe rozwiązanie swojeego problemu.'),
        nl,
		nl,
		write('Koniec rozgrywki. Wpisz "halt. by opuścić grę."'),
		nl.

/*if you lose argument with keeper you dont get damaged, what if you lose against a professor*/
finish_loser :-
		nl,
		write('Twoje argumenty były obiecujące, ale niestety niewystarczające. Dziekan patrzy na ciebie z wyrazem rozczarowania, a jego głos staje się jeszcze bardziej chłodny i stanowczy.'),
		nl,
		write('"W tej sytuacji na myśl przychodzi mi tylko jedno rozwiązanie. Zostaniesz naszym nowym dozorcą."'),
		nl,
		write('Zanim zdążysz zaprotestować, zostajesz wciągnięty w machinę, której nie udało ci się pokonać. Uczelnia, której kiedyś byłeś częścią, staje się twoim więzieniem.'),
		nl,
		nl,
		write('Koniec rozgrywki. Wpisz "halt. by opuścić grę."').

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
        write('OK.'),
        !, nl.

take(_) :-
        write('Nie ma tego tutaj.'),
        nl.

/* These rules define the direction letters as calls to go/1. */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

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
        i_am_at(Place),
        describe(Place),
		find_directions(Place),
        nl,
        notice_objects_at(Place),
        nl.

/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
        object_at(X, Place),
        write('Znajdujesz '), write(X), write('.'), nl,
        fail.

notice_objects_at(_).

notice_npcs_at(Place) :-
        npc_at(X, Place),
        write('Spotykasz '), write(X), write('.'), nl,
        fail.

notice_npcs_at(_).

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(_) :- write('Idziesz korytarzem.'), nl.

find_directions(Place) :-
		findall(Direction, path(Place, Direction, _), Directions),
		write('Dostępne ruchy: '), write(Directions),
		nl.
