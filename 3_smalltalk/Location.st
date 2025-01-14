Object subclass: Location [
    | name floor items npcs directions |

    Location class >> new: aName floor: aFloor [
        ^super new initialize: aName floor: aFloor.
    ]

    initialize: aName floor: aFloor [
        name := aName.
        floor := aFloor.
        directions := Dictionary new.
        items := OrderedCollection new.
        npcs := OrderedCollection new.
    ]

    Location >> name [
        ^name.
    ]

    Location >> floor [
        ^floor.
    ]

    Location >> items [
        ^items.
    ]

    Location >> directions [
        ^directions.
    ]

    addItem: anItem [
        items add: anItem.
    ]

    addNPC: aNPC [
        npcs add: aNPC.
    ]
        
    removeNPCS [
        npcs := OrderedCollection new.
    ]


    addDirection: aDirection to: aLocation [
        directions at: aDirection put: aLocation.
    ]

    encounterNPC [
        | npc |

        npc := npcs isEmpty
            ifTrue: [nil]
            ifFalse: [npcs first].

        ^npc
    ]

    reverseDirectionFor: aDirection [
        | reverseMap |
        reverseMap := Dictionary new.
        reverseMap at: 'n' put: 's'.
        reverseMap at: 's' put: 'n'.
        reverseMap at: 'e' put: 'w'.
        reverseMap at: 'w' put: 'e'.
        reverseMap at: 'up' put: 'down'.
        reverseMap at: 'down' put: 'up'.
        ^reverseMap at: aDirection ifAbsent: [nil].
    ]

    addPath: fromDirection to: toLocation [
        | reverseDirection |
        self addDirection: fromDirection to: toLocation.

        reverseDirection := self reverseDirectionFor: fromDirection.
        reverseDirection ifNotNil: [
            toLocation addDirection: reverseDirection to: self.
        ].
    ]

    describe [
    | itemDescriptions npcDescriptions directionDescriptions |
    itemDescriptions := items isEmpty
        ifTrue: ['']
        ifFalse: [
            String streamContents: [:stream |
                items do: [:item |
                    stream nextPutAll: 'Znajdujesz ', item itemName, ' - ', item itemDescription; nextPutAll: '. '].
                stream contents size > 2
                    ifTrue: [stream contents copyFrom: 1 to: stream contents size - 2]
                    ifFalse: [stream contents].
            ].
        ].

        npcDescriptions := npcs isEmpty
            ifTrue: ['']
            ifFalse: [
                String streamContents: [:stream |
                    npcs do: [:npc |
                        stream nextPutAll: 'Spotykasz ', npc name, '. '.].
                    stream contents.
                ].
            ].
    
        directionDescriptions := directions isEmpty
            ifTrue: ['Nie możesz nigdzie pójść, jak to możliwe?']
    		ifFalse: [
        'Dostępne kierunki: ', (String streamContents: [:stream |
            | first |
            first := true.
            directions keysAndValuesDo: [:direction :location |
                first ifTrue: [
                    first := false.
                ] ifFalse: [
                    stream nextPutAll: ', '.
                ].
                stream nextPutAll: direction.
            ].
			stream nextPutAll: '.'.
        ])
    ].


        ^'Jesteś na ', floor asString, '. piętrze, korytarz ', name, '. ', '
', 
        (directionDescriptions isEmpty ifTrue: [''] ifFalse: [directionDescriptions, '
']),
        (itemDescriptions isEmpty ifTrue: [''] ifFalse: [itemDescriptions, '
']),
        (npcDescriptions isEmpty ifTrue: [''] ifFalse: [npcDescriptions, '
']).
    ]
]
