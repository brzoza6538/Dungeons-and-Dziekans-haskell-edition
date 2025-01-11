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

    addItem: anItem [
        items add: anItem.
    ]

    addNPC: aNPC [
        npcs add: aNPC.
    ]

    describe [
    ^'Jesteś na ', floor asString, '. piętrze, lokacja: ', name, ' ',
       (items isEmpty ifTrue: [''] ifFalse: ['Znajdujesz: ', items asCommaString]), ' ',
       (npcs isEmpty ifTrue: [''] ifFalse: ['Spotykasz: ', npcs asCommaString]).
]
]

Object subclass: GameMap [
    | locations paths |

    GameMap class >> new [
        ^super new initialize.
    ]

    initialize [    
        | start location1 location2 |
        locations := Dictionary new.
        paths := OrderedCollection new.

        start := Location new: 'start' floor: 1.
        location1 := Location new: 'hallway1' floor: 1.
        location2 := Location new: 'hallway2' floor: 1.

        self addLocation: start.
        self addLocation: location1.
        self addLocation: location2.

        self addPathFrom: start direction: 'e' to: location1.
        self addPathFrom: location1 direction: 'e' to: location2.
    ]

    locations [
        ^locations.
    ]

    addLocation: aLocation [
        locations at: aLocation name put: aLocation.
    ]

    addPathFrom: fromLocation direction: direction to: toLocation [
        paths add: { fromLocation. direction. toLocation }.
    ]

    moveInDirection: fromLocation direction: aDirection [
        | destination |
        destination := paths detect: [ :path |
            (path first name = fromLocation name) and: [ path second = aDirection ]
        ] ifNone: [ nil ].

        ^destination ifNotNil: [ destination third ] ifNil: [ nil ].
    ]
]
