Object subclass: Location [
    | name floor items npcs directions |

    Location class >> new: aName floor: aFloor [
        ^super new initialize: aName floor: aFloor.
    ]

    initialize: aName floor: aFloor [
        name := aName.
        floor := aFloor.
        directions := OrderedCollection new.
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

    addItem: anItem [
        items add: anItem.
    ]

    addNPC: aNPC [
        npcs add: aNPC.
    ]

    addDirection: aDirection [
        directions add: aDirection.
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
                    directions do: [:direction |
                        stream nextPutAll: direction.
                        direction = directions last
                            ifFalse: [stream nextPutAll: ', ']
                            ifTrue: [stream nextPutAll: '.'].
                    ].
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
