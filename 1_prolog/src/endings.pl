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