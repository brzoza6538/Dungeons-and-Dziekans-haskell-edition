FileStream fileIn: 'Item.st'.
FileStream fileIn: 'Player.st'.
FileStream fileIn: 'Location.st'.
FileStream fileIn: 'GameMap.st'.


Object subclass: Interaction [
    |isGameOver iStudent iNpc iCurrentLocation|

    Interaction class >> new: student metNpc: aNpc currentLocation: aCurrentLocation[
        ^self new 
            setStudent: student;
            setNpc: aNpc;
            setCurrentLocation: aCurrentLocation;
            setState.
    ]

    setState [
        isGameOver := false.
    ]

    setCurrentLocation: aCurrentLocation [
        iCurrentLocation := aCurrentLocation.
    ]

    setStudent: aStudent [
        iStudent := aStudent.
    ]

    setNpc: aNpc [
        iNpc := aNpc.
    ]

    student [
        ^iStudent.
    ]

    npc [
        ^iNpc.
    ]

    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ].
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine.
    ]

    interact [

        iNpc name = 'Automat' ifTrue: [
            self interactWithVendingMachine.
        ].

        iNpc name = 'Wykładowca' ifTrue: [
            self interactWithLecturer.

        ].

        iNpc name = 'Dziekan' ifTrue: [
            self interactWithDean.
        ].

        iNpc name = 'Dozorca' ifTrue: [
            self interactWithCustodian.
        ].
        ^{iCurrentLocation . iNpc . iStudent . isGameOver}
    ]

    attack [
        | randOffset damage isFightOver |
        isFightOver := false.
        [isFightOver] whileFalse: [
            randOffset := (Array with: 0 with: 1 with: 2 with: 3) atRandom.
            damage := (randOffset + iStudent attack) - iNpc defense.
            damage := damage > 0 ifTrue: [damage] ifFalse: [0]. 
            self printLines: { 'Zadajesz ', damage printString , ' obrażenia', 'zostało mu: ', (iNpc energy printString) }.
            iNpc energy: (iNpc energy - damage).


            randOffset := (Array with: 0 with: 1 with: 2 with: 3) atRandom.
            damage := (randOffset + iNpc attack) - iStudent defense.
            damage := damage > 0 ifTrue: [damage] ifFalse: [0]. 
            self printLines: { 'Zadano ci ', damage printString , ' obrażenia', 'zostało ci: ', (iStudent energy printString) }.
            iStudent energy: (iStudent energy - damage).

            (iStudent energy < 0) ifTrue: [
                self printLines: { 'Koniec gry. Zostałeś pokonany' }.
                isFightOver := true.
                isGameOver := true.
            ].

            (iNpc energy < 0) ifTrue: [
                iCurrentLocation removeNPCS.
                self printLines: { 'I jak stał. ', iNpc name , ' teraz już nie stoi' }.
                isFightOver := true.

                (iNpc name = 'Dozorca') ifTrue: [
                    self printLines: { 'Dozorca został pokonany, nic nie stoi ci na drodze do wyjścia. Ale nadal czujesz ogromny ciężar na sercu, wiesz, że to jeszcze nie koniec...', 'Koniec gry.' }.
                    isGameOver := true.
                ].

                (iNpc name = 'Dziekan') ifTrue: [
                    self printLines: { 'Dziekan został pokonany, nic nie stoi ci na drodze do wolności', 'Koniec gry.' }.
                    isGameOver := true.
                ].
            ].

        ].
    ]


    getItemFromMachine [
        | reward |

        iNpc inventory isEmpty ifTrue: [
            self printLines: { 'wydaje się że ktoś już skorzystał z okazji' }.
        ] ifFalse: [
            reward := iNpc inventory atRandom.
            iStudent addToInventory: reward.
            iNpc inventory: OrderedCollection new.

            self printLines: { 'Wpisujesz kod, jednak automat wydaje się nie przejmować zupełnie twoimi instrukcjami podając ci coś innego. Wraz z wyrzuceniem przedmiotu, światełko w automacie zaczyna intensywnie migać, aż w końcu gaśnie. '. 'otrzymujesz ', reward itemName }.
        ].
    ]

    interactWithVendingMachine [
        |cmd isUnknown|
        [isGameOver] whileFalse: [
            isUnknown := true.

            self printLines: { 
                'Automat obok buczy nieznacznie, czy masz na coś ochotę?'. 
                '1  -- Potrzebuję kopa, może jakiś energetyk?'. 
                '2  -- Przydałyby mi się jakieś lepsze notatki.'. 
                '3  -- Chyba nic nie potrzebuję...'.
                '4  -- Uderz w automat, może się uda wyciągnąć kilka rzeczy' 
            }.

            cmd := self readCommand.  "Wczytanie nowej komendy po pokazaniu opcji"

            cmd = '1' ifTrue: [ 
                self getItemFromMachine.  "Nagroda"
                isUnknown := false.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].
            cmd = '2' ifTrue: [ 
                self getItemFromMachine.  "Nagroda"
                isUnknown := false.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}

            ].
            cmd = '3' ifTrue: [ 
                self printLines: {'Może rzeczywiście to nie czas na zakupy.'}.
                isUnknown := false.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].
            cmd = '4' ifTrue: [ 
                self printLines: { 
                    'Maszyna wydaje się buczyć "Ach, wreszcie godny oponent..."'. 
                    'Po czym wybucha drastycznie ciebie raniąc. Sfrustrowany dziekan podziwia twoje poświęcenie oglądając jak twoje nieprzytomne ciało jest wywożone z wydziału ambulansem bez uiszczenia opłaty'. 
                    'Koniec gry. Nie musisz się już martwić warunkami, właściwie nie martwisz się już o nic...'
                }.
                isGameOver := true.
                isUnknown := false.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].
            
            isUnknown ifTrue: [
                self printLines: {'Wklepujesz swój wybór w automat, jednak on ani drgnie. Czy na pewno się nie pomyliłeś?'}.
            ].
        ].
    ]


    interactWithLecturer [
        |cmd isUnknown charismaChance|
        [isGameOver] whileFalse: [
            isUnknown := true.

            self printLines: { 
                'Witasz wykładowcę, który zaczyna mówić o twoich problemach w nauce.'.
                'Pragnie cię odpytać czy jedynie porozmawiać?'.
                '1  -- Starasz się wytłumaczyć'.
                '2  -- Obrażony wyzywasz go'.
                '3  -- Próbujesz go zmanipulować by ci pomógł'.
            }.

            cmd := self readCommand.  "Wczytanie nowej komendy po pokazaniu opcji"

            cmd = '1' ifTrue: [ 
                self printLines: {'Wykładowca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie.'}.
                isUnknown := false.
            ].
            cmd = '2' ifTrue: [ 
                self printLines: {'Wykładowca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: "Dosć padło słów, czas podyskutować"'}.
                isUnknown := false.
                self attack.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            cmd = '3' ifTrue: [ 
                isUnknown := false.
                charismaChance := (Array with: 0 with: 1) atRandom.

                (charismaChance = 1 and: [iStudent charisma > 0]) ifTrue: [
                    self printLines: {'Dzięki odrobinie charyzmy wykładowca zdradza ci sekret. Przed wyjściem powinien właśnie teraz stać dozorca. Jest to rozsądny człowiek, może uda ci się z nim dogadać.'}.
                    iCurrentLocation initializeCustodianNPC.
                ] ifFalse: [
                    self printLines: {'Starasz się zrobić dobre wrażenie i odpowiadasz mu "Dzień dobry panie magistrze doktorze doktorancki od spraw bardzo ważnych". Nie przemyślałeś jednak, że przywitanie może nie być najmądrzejszym pomysłem, kiedy wykładowca oczekuje od ciebie wyjaśnień. Twarz mu nagle pochmurnieje, a z jego ust zaczynają się sypać pytania dotyczące twojej wiedzy.'}.
                    self attack.
                ].

                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            isUnknown ifTrue: [
                self printLines: { 'Wykładowca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi' }.
            ].
        ].
    ]
    

    
    interactWithCustodian [
        |cmd isUnknown charismaChance|
        [isGameOver] whileFalse: [
            isUnknown := true.

            self printLines: { 
                'Zauważasz starszego mężczyznę stojącego przed wejściem. Czy zawsze tutaj stał? Jego posiwiałe włosy i zmarszczki opowiadają historię o zmarnowanej młodości i życiu pełnego stresu i żalów. Jednak nie masz czasu na sympatię. Prosisz go żeby cię wypuścił, jednak on zdaje się ciebie nie słuchać.'.
                '1  -- Tłumaczysz swoją sytuację'.
                '2  -- Obrażony, wybuchasz oraz każesz mu otworzyć drzwi w mniej niż uprzejmy sposób'.
                '3  -- Próbujesz ponownie wykorzystać swoją charyzmę'.
            }.

            cmd := self readCommand.  "Wczytanie nowej komendy po pokazaniu opcji"

            cmd = '1' ifTrue: [ 
                self printLines: {'Dozorca wciąż siedzi bez ruchu, ignoruje twoje tłumaczenia czy nawet ich nie słyszy?'}.
                isUnknown := false.
            ].
            cmd = '2' ifTrue: [ 
                self printLines: {'Dozorca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padło słów, czas podyskutować"'}.
                isUnknown := false.
                self attack.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            cmd = '3' ifTrue: [ 
                isUnknown := false.
                charismaChance := (Array with: 0 with: 1) atRandom.

                (charismaChance = 1 and: [iStudent charisma > 0]) ifTrue: [
                    self printLines: {
                            'Odrobina uprzejmości i dozorca jakby nagle ożył. Jednak po pewnym pytaniu zamiera ci serce.'.
                            '- "Hej, czy to aby nie ty nie opłaciłeś warunku i chcesz teraz wyjść na pewniaka, bez konfrontacji z dziekanem?" - pyta'.
                            'Milczysz, ale boisz się najgorszego'.
                            '"Wiesz", zaczyna powoli, "właściwie to przypominasz mi mnie samego z czasów studiów. Też walczyłem z systemem, dyskutowałem, kombinowałem, jak ominąć regulamin. Niestety nie udało się i zobacz, jak skończyłem. Nie chciałbym, żeby kogoś jeszcze to spotkało."'.
                            'Dozorca ustąpił, a ty wychodzisz z uczelni.'.
                            ''.
                            'Udało Ci się ominąć stróża i wydostajesz się z budynku. Czujesz rosnącą ulgę z każdym krokiem oddalającym cię od biura dziekana. Na moment wydaje się, że wszystko jest w porządku - jakbyś naprawdę przechytrzył system. Twoje długi jednak nie zniknęły i nie będzie można przed nimi uciekać w nieskończoność. Odsetki od twojego długu wciąż rosną, a kolejne listy z uczelni zaczną spływać szybciej, niż zdążysz znaleźć kryjówkę, zmienić tożsamość lub znaleźć prawdziwe rozwiązanie swojeego problemu.'.
                            'Koniec gry'                
                    }.
                    isGameOver := true.
                ] ifFalse: [
                    self printLines: {' Jak się okazuje, dozorca nie ma najmniejszej ochoty z tobą rozmawiać, a twoje natręctwo tylko go zirytowało.'}.
                    self attack.
                ].
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            isUnknown ifTrue: [
                self printLines: { 'Bełkoczesz coś pod nosem, oczywiście dozorcy to nie rusza. Musisz podjąć jakąś decyzję' }.
            ].
        ].
    ]


    
    interactWithDean [
        |cmd isUnknown charismaChance golden|
        [isGameOver] whileFalse: [
            isUnknown := true.

            self printLines: { 
                'Zauważasz niewielki błysk na końcu korytarza. Z zaciekawieniem postanawisz podejść bliżej, jednak szybko żałujesz tej decyzji. Z cienia wyłania się dziekan szczerząc się do ciebie. Witasz go, jednak ten nie wydaje się chętny do słuchania wymówek.'.
                '"Twoje żałosne próby zdania semestru kosztowały mnie już wystarczająco dużo czasu. Twój czas dobiegł końca" - mówi z sadystyczną satysfakcją'.
                '1  -- Tłumaczysz swoją sytuację'.
                '2  -- Obrażony nonszalancją dziekana wybuchasz i mówisz dwa razy więcej niż powinieneś'.
                '3  -- Próbujesz wykorzystać swój urok osobisty'.
                '4  -- Mówisz dziekanowi że masz ze sobą legendarny złoty strzał'
            }.

            cmd := self readCommand.  "Wczytanie nowej komendy po pokazaniu opcji"

            cmd = '1' ifTrue: [ 
                self printLines: {'Dziekan potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie'}.
                isUnknown := false.
            ].
            cmd = '2' ifTrue: [ 
                self printLines: {'Dziekan wydaje się obrażony, możliwe że powiedziałeś za dużo"'}.
                isUnknown := false.
                self attack.
                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            cmd = '3' ifTrue: [ 
                isUnknown := false.
                charismaChance := (Array with: 0 with: 1) atRandom.

                (charismaChance = 1 and: [iStudent charisma > 1]) ifTrue: [
                    self printLines: {
                            'Dziekan słucha cię uważnie, twoje idealnie dobrane odzienie, naturalna charyzma i umniejętność zmyślania historii w czasie rzeczywistym, sprawiają że nie może ciebie nie wysłuchać'.
                            'Po chwili namysłu postanawia ci pomóc. "Możesz uznać swój dług za spłacony" mówi'.
                            'Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że przyszłość jest w twoich rękach'                
                    }.
                    isGameOver := true.
                ] ifFalse: [
                    self printLines: {' Dziekan wydaje się słuchać ciebie z uwagą, jednak w połowie twojej historii o tragedii jak twój trzeci pies zjadł również twój laptop podczas pogrzebu twojej babci, zaczyna się z ciebie śmiać. Nie rozwiążesz tego rozmową.'}.
                    self attack.
                ].

                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].

            cmd = '4' ifTrue: [ 
                isUnknown := false.


                golden := iStudent inventory detect: [:item | item itemName = 'Złoty strzał'] ifNone: [nil].
                golden ifNil: [
                    self printLines: {'Dziekan stoi przez chwilę oniemiały, był pewny, śmieje się z ciebie i pyta, czy to też pies ci zjadł.'}.
                ] ifNotNil: [
                    self printLines: {'Dziekan stoi przez chwilę oniemiały, był pewny, że to tylko legenda. Jednak wie dobrze, że jest to coś, co musi uhonorować. Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz, że masz przyszłość.'}.
                    isGameOver := true.
                ].

                ^{iCurrentLocation . iNpc . iStudent . isGameOver}
            ].


            isUnknown ifTrue: [
                self printLines: { 'Dziekan nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi' }.
            ].
        ].
    ]
]
