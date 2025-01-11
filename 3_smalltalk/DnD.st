"Dungeons and Dziekans
Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska"

Object subclass: Game [
    | isGameOver map currentLocation |

    introductionText := #(
        'Witaj w Dungeons and Dziekans. Jestes studentem, który niedawno obronil prace inzynierska. Przy odbiorze dyplomu z dziekanatu wyszlo na jaw, ze nie oplaciles warunku. Znajdz dziekana i sie z nim rozmow lub ucieknij niepostrzezenie.'
    ).

    instructionsText := #(
        ''
        'Dostepne akcje to:'
        'start                  -- Rozpocznij gre.'
        'go n                   -- Idz w kierunku polnocnym.'
        'go e                   -- Idz w kierunku wschodnim.'
        'go w                   -- Idz w kierunku zachodnim.'
        'go s                   -- Idz w kierunku poludniowym.'
        'go up                  -- Idz na gore.'
        'go down                -- Idz na dol.'
        'take <item_name>       -- Podnies przedmiot.'
        'drop <item_name>       -- Upusc przedmiot.'
        'look                   -- Rozejrzyj sie.'
        'stats                  -- Wyswietl swoje statystyki.'
        'inventory              -- Wyswietl swoje przedmioty.'
        'instructions           -- Wyswietl instrukcje ponownie.'
        'quit                   -- Zakoncz rozgrywke i wyjdz.'
        ''
    ).

    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        isGameOver := false.
        map := GameMap new.
        currentLocation := map locations first.  "Przypisujemy pierwszą lokację z listy"
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ].
    ]

    printIntroduction [
        self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine.
    ]

    "Run the game."
    run [
        | cmd isUnknown result newLocation direction |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false.
            ].

            (cmd startsWith: 'go ') ifTrue: [
                direction := cmd copyFrom: 4 to: cmd size.
                newLocation := map moveInDirection: currentLocation direction: direction.
                newLocation ifNotNil: [
                    currentLocation := newLocation.
                    self printLines: { 'Przeszedłeś do: ', currentLocation name }.
                ] ifNil: [
                    self printLines: { 'Nie można tam przejść.' }.
                ].
                isUnknown := false.
            ].

            cmd = 'look' ifTrue: [
                self printLines: { currentLocation describe }.
                isUnknown := false.
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false.
            ].

            isUnknown ifTrue: [
                self printLines: #('Nieznana komenda.' '').
            ].
        ]
    ]
]

Game new run.