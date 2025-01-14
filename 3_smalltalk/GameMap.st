Object subclass: GameMap [
    | locations |

    GameMap class >> new [
        | g |
        g := super new.
        g initialize.
        ^g
    ]

    initialize [
        locations := Dictionary new.
        self initializeLocations.
        self initializePaths.
		self initializeItemsNPCs.
    ]

    initializeLocations [
        1 to: 5 do: [:row |
            #('a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j') doWithIndex: [:colName :col |
                | locationName key location |
                locationName := row asString, '_', colName.
                key := '1_', locationName.
                location := Location new: locationName floor: 1.
                locations at: key put: location.
            ].
        ].

		
        1 to: 5 do: [:row |
            #('a' 'b' 'c' 'd' 'e') doWithIndex: [:colName :col |
                | locationName key location |
                locationName := row asString, '_', colName.
                key := '2_', locationName.
                location := Location new: locationName floor: 2.
                locations at: key put: location.
            ].
        ].
    ]

	initializePaths [
		self addPathBetween: '1_1_a' and: '1_1_b' inDirection: 'e'.
		self addPathBetween: '1_1_b' and: '1_1_c' inDirection: 'e'.
		self addPathBetween: '1_1_c' and: '1_2_c' inDirection: 's'.
		self addPathBetween: '1_2_c' and: '1_2_b' inDirection: 'w'.
		self addPathBetween: '1_1_a' and: '1_2_a' inDirection: 's'.
		self addPathBetween: '1_2_a' and: '1_3_a' inDirection: 's'.
		self addPathBetween: '1_3_a' and: '1_3_b' inDirection: 'e'.
		self addPathBetween: '1_3_b' and: '1_4_b' inDirection: 's'.
		self addPathBetween: '1_4_b' and: '1_4_c' inDirection: 'e'.
		self addPathBetween: '1_4_c' and: '1_3_c' inDirection: 'n'.
		self addPathBetween: '1_3_c' and: '1_3_d' inDirection: 'e'.
		self addPathBetween: '1_4_b' and: '1_4_a' inDirection: 'w'.
		self addPathBetween: '1_4_a' and: '1_5_a' inDirection: 's'.
		self addPathBetween: '1_5_a' and: '1_5_b' inDirection: 'e'.
		self addPathBetween: '1_5_b' and: '1_5_c' inDirection: 'e'.
		self addPathBetween: '1_5_c' and: '1_5_d' inDirection: 'e'.
		self addPathBetween: '1_5_d' and: '1_4_d' inDirection: 'n'.
		self addPathBetween: '1_4_d' and: '1_4_e' inDirection: 'e'.
		self addPathBetween: '1_4_e' and: '1_5_e' inDirection: 's'.
		self addPathBetween: '1_5_e' and: '1_5_f' inDirection: 'e'.
		self addPathBetween: '1_4_e' and: '1_3_e' inDirection: 'n'.
		self addPathBetween: '1_3_e' and: '1_3_f' inDirection: 'e'.
		self addPathBetween: '1_3_f' and: '1_4_f' inDirection: 's'.
		self addPathBetween: '1_4_f' and: '1_4_g' inDirection: 'e'.
		self addPathBetween: '1_3_e' and: '1_2_e' inDirection: 'n'.
		self addPathBetween: '1_2_e' and: '1_2_d' inDirection: 'w'.
		self addPathBetween: '1_2_d' and: '1_1_d' inDirection: 'n'.
		self addPathBetween: '1_1_d' and: '1_1_e' inDirection: 'e'.
		self addPathBetween: '1_1_e' and: '1_1_f' inDirection: 'e'.
		self addPathBetween: '1_1_f' and: '1_2_f' inDirection: 's'.
		self addPathBetween: '1_2_f' and: '1_2_g' inDirection: 'e'.
		self addPathBetween: '1_2_g' and: '1_3_g' inDirection: 's'.
		self addPathBetween: '1_3_g' and: '1_3_h' inDirection: 'e'.
		self addPathBetween: '1_3_h' and: '1_2_h' inDirection: 'n'.
		self addPathBetween: '1_1_f' and: '1_1_g' inDirection: 'e'.
		self addPathBetween: '1_1_g' and: '1_2_g' inDirection: 's'.
		self addPathBetween: '1_1_g' and: '1_1_h' inDirection: 'e'.
		self addPathBetween: '1_1_h' and: '1_1_i' inDirection: 'e'.
		self addPathBetween: '1_1_i' and: '1_1_j' inDirection: 'e'.
		self addPathBetween: '1_1_j' and: '1_2_j' inDirection: 's'.
		self addPathBetween: '1_2_j' and: '1_2_i' inDirection: 'w'.
		self addPathBetween: '1_2_i' and: '1_3_i' inDirection: 's'.
		self addPathBetween: '1_3_i' and: '1_4_i' inDirection: 's'.
		self addPathBetween: '1_4_i' and: '1_4_j' inDirection: 'e'.
		self addPathBetween: '1_4_j' and: '1_3_j' inDirection: 'n'.
		self addPathBetween: '1_4_i' and: '1_4_h' inDirection: 'w'.
		self addPathBetween: '1_4_h' and: '1_5_h' inDirection: 's'.
		self addPathBetween: '1_5_h' and: '1_5_g' inDirection: 'w'.
		self addPathBetween: '1_5_h' and: '1_5_i' inDirection: 'e'.
		self addPathBetween: '1_5_i' and: '1_5_j' inDirection: 'e'.
		self addPathBetween: '2_1_a' and: '2_1_b' inDirection: 'e'.
		self addPathBetween: '2_1_b' and: '2_1_c' inDirection: 'e'.
		self addPathBetween: '2_1_c' and: '2_2_c' inDirection: 's'.
		self addPathBetween: '2_2_c' and: '2_2_b' inDirection: 'w'.
		self addPathBetween: '2_2_b' and: '2_3_b' inDirection: 's'.
		self addPathBetween: '2_3_b' and: '2_4_b' inDirection: 's'.
		self addPathBetween: '2_4_b' and: '2_4_c' inDirection: 'e'.
		self addPathBetween: '2_4_c' and: '2_4_d' inDirection: 'e'.
		self addPathBetween: '2_4_b' and: '2_5_b' inDirection: 's'.
		self addPathBetween: '2_5_b' and: '2_5_a' inDirection: 'w'.
		self addPathBetween: '2_5_a' and: '2_4_a' inDirection: 'n'.
		self addPathBetween: '2_4_a' and: '2_3_a' inDirection: 'n'.
		self addPathBetween: '2_3_a' and: '2_2_a' inDirection: 'n'.
		self addPathBetween: '2_2_c' and: '2_3_c' inDirection: 's'.
		self addPathBetween: '2_3_c' and: '2_3_d' inDirection: 'e'.
		self addPathBetween: '2_3_d' and: '2_2_d' inDirection: 'n'.
		self addPathBetween: '2_2_d' and: '2_1_d' inDirection: 'n'.
		self addPathBetween: '2_3_d' and: '2_3_e' inDirection: 'e'.
		self addPathBetween: '2_3_e' and: '2_2_e' inDirection: 'n'.
		self addPathBetween: '2_2_e' and: '2_1_e' inDirection: 'n'.
		self addPathBetween: '2_3_e' and: '2_4_e' inDirection: 's'.
		self addPathBetween: '2_4_e' and: '2_5_e' inDirection: 's'.
		self addPathBetween: '2_5_e' and: '2_5_d' inDirection: 'w'.
		self addPathBetween: '2_5_d' and: '2_5_c' inDirection: 'w'.
        self addPathBetween: '1_4_g' and: '2_1_a' inDirection: 'up'.
	]

    initializeItemsNPCs [
        | poorNotes outdatedRegulations poorNotesLocation outdatedRegulationsLocation betterNotes upToDateRegulations formalWear energyDrink vendingMachine lecturer jackpot dean jackpotLocations vmLocations vmLocationName vmLocation availableLocations lecturerLocationName lecturerLocation jackpotLocationName jackpotLocation deanLocation custodian |

        poorNotes := Item new: 'Słabe notatki' description: 'Notatki pozostawione przez studenta pierwszego roku. Nie dają dużo, ale lepszy rydz niż nic. (+2 ataku)' stat: 'Attack' value: 2.
        outdatedRegulations := Item new: 'Nieaktualny regulamin studiów' description: 'Zbiór kartek, na których widnieje wielka liczba ''2005''. Może coś jest jeszcze aktualne? (+2 obrony)' stat: 'Defense' value: 2.

        poorNotesLocation := locations at: '1_2_b' ifAbsent: [nil].
        poorNotesLocation addItem: outdatedRegulations.
        outdatedRegulationsLocation := locations at: '1_3_d' ifAbsent: [nil].
        outdatedRegulationsLocation addItem: poorNotes.

        betterNotes := Item new: 'Lepsze notatki' description: 'Notatki pozostawione przez studenta pierwszego roku, ale studiów magisterskich, zmagającego się z tym samym problemem umorzenia warunku (+3 ataku).' stat: 'Attack' value: 3.
        upToDateRegulations := Item new: 'Nowy regulamin' description: 'Zbiór zapisów, którymi możesz się osłonić przed słownymi ciosami pracowników uczelni (+3 obrony)' stat: 'Defense' value: 3.
        formalWear := Item new: 'Toga' description: 'Modny dodatek. Ludzie będą traktować cię poważnie, nawet jeśli nie masz nic mądrego do powiedzenia (+1 charyzmy)' stat: 'Charisma' value: 1.
        energyDrink := Item new: 'Energetyk' description: 'Niezastąpiony w momentach kryzysowych. Daje energię, np.: by dokończyć noc przed oddaniem (prawie) każdy projekt (+10 energii)' stat: 'Energy' value: 10.

        vendingMachine := Player new: 'Automat' energy: 0 attack: 0 defense: 0 inventory: (OrderedCollection with: betterNotes with: upToDateRegulations with: formalWear with: energyDrink).

        vmLocations := #('1_3_e' '1_2_g' '1_5_g' '1_3_j').
        vmLocationName := vmLocations atRandom.
        vmLocation := locations at: vmLocationName ifAbsent: [nil].
        vmLocation addNPC: vendingMachine.

        lecturer := Player new: 'Wykładowca' energy: 20 attack: 5 defense: 0 inventory: #().
        availableLocations := vmLocations select: [:each | each ~= vmLocationName].
        lecturerLocationName := availableLocations atRandom.
        lecturerLocation := locations at: lecturerLocationName ifAbsent: [nil].
        lecturerLocation addNPC: lecturer.

        jackpot := Item new: 'Złoty strzał' description: 'Mówi się o nim jak o legendzie, podobno posiadacz złotego strzału może za darmo anulować wszystkie warunki.' stat: nil value: 0.

        jackpotLocations := #('2_4_d' '2_2_a' '2_1_d' '2_1_e' '2_5_c').
        jackpotLocationName := jackpotLocations atRandom.
        jackpotLocation := locations at: jackpotLocationName ifAbsent: [nil].
        jackpotLocation addItem: jackpot.

        dean := Player new: 'Dziekan' energy: 50 attack: 10 defense: 4 inventory: #().

        jackpotLocations do: [:deanLocationName |
            deanLocation := locations at: deanLocationName ifAbsent: [nil].
            deanLocation addNPC: dean.
        ].
    ]

initializeCustodianNPC [
    | custodian custodianLocation |

    custodian := Player new: 'Dozorca' energy: 20 attack: 3 defense: 3 inventory: (OrderedCollection new).
    custodianLocation := locations at: '1_5_i' ifAbsent: [nil].
    custodianLocation addNPC: custodian.

]


    addPathBetween: location1key and: location2key inDirection: aDirection [
        | location1 location2 reverseDirection |
        location1 := locations at: location1key ifAbsent: [nil].
        location2 := locations at: location2key ifAbsent: [nil].

        location1 addDirection: aDirection to: location2.

        reverseDirection := location1 reverseDirectionFor: aDirection.
        reverseDirection ifNotNil: [
            location2 addDirection: reverseDirection to: location1.
        ].
    ]

    locations [
        ^locations.
    ]

    moveInDirection: aLocation direction: aDirection [
        | destinationLocation |
        destinationLocation := aLocation directions at: aDirection ifAbsent: [nil].
        ^destinationLocation.
    ]
]
