Object subclass: Location [
    | name floor items npcs |

    Location class >> new: aName floor: aFloor [
        ^super new initialize: aName floor: aFloor.
    ]

    initialize: aName floor: aFloor [
        name := aName.
        floor := aFloor.
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

    describe [
    | itemDescriptions npcDescriptions |
    itemDescriptions := items isEmpty
        ifTrue: ['Nie widzisz tutaj żadnych przedmiotów.']
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
            ifTrue: ['Nie ma tu nikogo.']
            ifFalse: [
                String streamContents: [:stream |
                    npcs do: [:npc |
                        stream nextPutAll: 'Spotykasz ', npc npcName, '. '.].
                    stream contents.
                ].
            ].

        ^'Jestes na ', floor asString, '. pietrze, korytarz ', name, '. ', 
'
', itemDescriptions, 
'
', npcDescriptions.
    ]
]
