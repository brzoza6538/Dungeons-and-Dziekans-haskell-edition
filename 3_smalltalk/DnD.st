"Dungeons and Dziekans
Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska"

FileStream fileIn: 'GameMap.st'.
FileStream fileIn: 'Item.st'.
FileStream fileIn: 'NPC.st'.
FileStream fileIn: 'Location.st'.

Object subclass: Game [
    | isGameOver map currentLocation inventory |

    introductionText := #(
        'Witaj w Dungeons and Dziekans. Jesteś studentem, który niedawno obronił pracę inżynierską. Przy odbiorze dyplomu z dziekanatu wyszło na jaw, że nie opłaciłeś warunku. Znajdź dziekana i się z nim rozmów lub ucieknij niepostrzeżenie.'
    ).

    instructionsText := #(
        ''
        'Dostępne akcje to:'
        'start                  -- Rozpocznij grę.'
        'go n                   -- Idź w kierunku północnym.'
        'go e                   -- Idź w kierunku wschodnim.'
        'go w                   -- Idź w kierunku zachodnim.'
        'go s                   -- Idź w kierunku południowym.'
        'go up                  -- Idź na górę.'
        'go down                -- Idź na dół.'
        'take <item_name>       -- Podnieś przedmiot.'
        'drop <item_name>       -- Upuść przedmiot.'
        'look                   -- Rozejrzyj się.'
        'stats                  -- Wyświetl swoje statystyki.'
        'inventory              -- Wyświetl swoje przedmioty.'
        'instructions           -- Wyświetl instrukcje ponownie.'
        'quit                   -- Zakończ rozgrywkę i wyjdź.'
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
        currentLocation := map locations first.
        inventory := OrderedCollection new.
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

    printInventory [
        | inventoryDescriptions |
		inventoryDescriptions := inventory isEmpty
		ifTrue: ['Twój ekwipunek jest pusty.']
		ifFalse: [
			String streamContents: [:stream |
				inventory do: [:item |
					stream nextPutAll: 'Twój ekwipunek: ', item itemName, ' - ', item itemDescription; nextPutAll: '. '].
				stream contents size > 2
					ifTrue: [stream contents copyFrom: 1 to: stream contents size - 2]
					ifFalse: [stream contents].
		].
	].
    
    self printLines: { inventoryDescriptions }.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine.
    ]

    "Run the game."
    run [
        | cmd isUnknown result newLocation direction itemName itemToTake itemToDrop |

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
                    self printLines: { 'Idziesz w kierunku ', direction, '.' }.
                    self printLines: { '' }.
                	self printLines: { currentLocation describe }.
                ] ifNil: [
                    self printLines: { 'Nie można tam przejść.' }.
                ].
                isUnknown := false.
            ].

            cmd = 'look' ifTrue: [
                self printLines: { currentLocation describe }.
                isUnknown := false.
            ].
            
            (cmd startsWith: 'take ') ifTrue: [
                itemName := cmd copyFrom: 6 to: cmd size.
                itemToTake := currentLocation items detect: [:item | item itemName = itemName] ifNone: [nil].
                itemToTake ifNotNil: [
                    inventory add: itemToTake.
                    currentLocation items remove: itemToTake.
                    self printLines: { 'Podniosłeś: ', itemToTake itemName }.
                ] ifNil: [
                    self printLines: { 'Nie znaleziono przedmiotu o nazwie ', itemName }.
                ].
                isUnknown := false.
            ].

            (cmd startsWith: 'drop ') ifTrue: [
                itemName := cmd copyFrom: 6 to: cmd size.
                itemToDrop := inventory detect: [:item | item itemName = itemName] ifNone: [nil].
                itemToDrop ifNotNil: [
                    inventory remove: itemToDrop.
                    currentLocation addItem: itemToDrop.
                    self printLines: { 'Upuściłeś: ', itemToDrop itemName }.
                ] ifNil: [
                    self printLines: { 'Nie masz tego przedmiotu w ekwipunku.' }.
                ].
                isUnknown := false.
            ].

            cmd = 'inventory' ifTrue: [
                self printInventory.
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
