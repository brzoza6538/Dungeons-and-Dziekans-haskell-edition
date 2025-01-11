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
        ^'Jesteś na ', floor asString, '. piętrze, korytarz ', name, ' ' ,
           (items isEmpty ifTrue: [''] ifFalse: ['Znajdujesz: ', items asCommaString]), ' ' ,
           (npcs isEmpty ifTrue: [''] ifFalse: ['Spotykasz: ', npcs asCommaString]).
    ]
]

Object subclass: GameMap [
    | locations paths |

    GameMap class >> new [
        ^super new initialize.
    ]

    initializeFirstFloor [    
        | location1_1_a location1_1_b location1_1_c location1_1_d location1_1_e location1_1_f location1_1_g location1_1_h location1_1_i location1_1_j
		location1_2_a location1_2_b location1_2_c location1_2_d location1_2_e location1_2_f location1_2_g location1_2_h location1_2_i location1_2_j
		location1_3_a location1_3_b location1_3_c location1_3_d location1_3_e location1_3_f location1_3_g location1_3_h location1_3_i location1_3_j
		location1_4_a location1_4_b location1_4_c location1_4_d location1_4_e location1_4_f location1_4_g location1_4_h location1_4_i location1_4_j
		location1_5_a location1_5_b location1_5_c location1_5_d location1_5_e location1_5_f location1_5_g location1_5_h location1_5_i location1_5_j|

        location1_1_a := Location new: '1_a' floor: 1.
        location1_1_b := Location new: '1_b' floor: 1.
        location1_1_c := Location new: '1_c' floor: 1.
        location1_1_d := Location new: '1_d' floor: 1.
        location1_1_e := Location new: '1_e' floor: 1.
        location1_1_f := Location new: '1_f' floor: 1.
        location1_1_g := Location new: '1_g' floor: 1.
        location1_1_h := Location new: '1_h' floor: 1.
        location1_1_i := Location new: '1_i' floor: 1.
        location1_1_j := Location new: '2_j' floor: 1.
        location1_2_a := Location new: '2_a' floor: 1.
        location1_2_b := Location new: '2_b' floor: 1.
        location1_2_c := Location new: '2_c' floor: 1.
        location1_2_d := Location new: '2_d' floor: 1.
        location1_2_e := Location new: '2_e' floor: 1.
        location1_2_f := Location new: '2_f' floor: 1.
        location1_2_g := Location new: '2_g' floor: 1.
        location1_2_h := Location new: '2_h' floor: 1.
        location1_2_i := Location new: '2_i' floor: 1.
        location1_2_j := Location new: '2_j' floor: 1.
        location1_3_a := Location new: '3_a' floor: 1.
        location1_3_b := Location new: '3_b' floor: 1.
        location1_3_c := Location new: '3_c' floor: 1.
        location1_3_d := Location new: '3_d' floor: 1.
        location1_3_e := Location new: '3_e' floor: 1.
        location1_3_f := Location new: '3_f' floor: 1.
        location1_3_g := Location new: '3_g' floor: 1.
        location1_3_h := Location new: '3_h' floor: 1.
        location1_3_i := Location new: '3_i' floor: 1.
        location1_3_j := Location new: '3_j' floor: 1.
        location1_4_a := Location new: '4_a' floor: 1.
        location1_4_b := Location new: '4_b' floor: 1.
        location1_4_c := Location new: '4_c' floor: 1.
        location1_4_d := Location new: '4_d' floor: 1.
        location1_4_e := Location new: '4_e' floor: 1.
        location1_4_f := Location new: '4_f' floor: 1.
        location1_4_g := Location new: '4_g' floor: 1.
        location1_4_h := Location new: '4_h' floor: 1.
        location1_4_i := Location new: '4_i' floor: 1.
        location1_4_j := Location new: '4_j' floor: 1.
        location1_5_a := Location new: '5_a' floor: 1.
        location1_5_b := Location new: '5_b' floor: 1.
        location1_5_c := Location new: '5_c' floor: 1.
        location1_5_d := Location new: '5_d' floor: 1.
        location1_5_e := Location new: '5_e' floor: 1.
        location1_5_f := Location new: '5_f' floor: 1.
        location1_5_g := Location new: '5_g' floor: 1.
        location1_5_h := Location new: '5_h' floor: 1.
        location1_5_i := Location new: '5_i' floor: 1.
        location1_5_j := Location new: '5_j' floor: 1.

        "Dodajemy lokacje do kolekcji"
        self addLocation: location1_1_a.
        self addLocation: location1_1_b.
        self addLocation: location1_1_c.
        self addLocation: location1_1_d.
        self addLocation: location1_1_e.
        self addLocation: location1_1_f.
        self addLocation: location1_1_g.
        self addLocation: location1_1_h.
        self addLocation: location1_1_i.
        self addLocation: location1_1_j.
        self addLocation: location1_2_a.
        self addLocation: location1_2_b.
        self addLocation: location1_2_c.
        self addLocation: location1_2_d.
        self addLocation: location1_2_e.
        self addLocation: location1_2_f.
        self addLocation: location1_2_g.
        self addLocation: location1_2_h.
        self addLocation: location1_2_i.
        self addLocation: location1_2_j.
        self addLocation: location1_3_a.
        self addLocation: location1_3_b.
        self addLocation: location1_3_c.
        self addLocation: location1_3_d.
        self addLocation: location1_3_e.
        self addLocation: location1_3_f.
        self addLocation: location1_3_g.
        self addLocation: location1_3_h.
        self addLocation: location1_3_i.
        self addLocation: location1_3_j.
        self addLocation: location1_4_a.
        self addLocation: location1_4_b.
        self addLocation: location1_4_c.
        self addLocation: location1_4_d.
        self addLocation: location1_4_e.
        self addLocation: location1_4_f.
        self addLocation: location1_4_g.
        self addLocation: location1_4_h.
        self addLocation: location1_4_i.
        self addLocation: location1_4_j.
        self addLocation: location1_5_a.
        self addLocation: location1_5_b.
        self addLocation: location1_5_c.
        self addLocation: location1_5_d.
        self addLocation: location1_5_e.
        self addLocation: location1_5_f.
        self addLocation: location1_5_g.
        self addLocation: location1_5_h.
        self addLocation: location1_5_i.
        self addLocation: location1_5_j.

		self addPathFrom: location1_1_a direction: 'e' to: location1_1_b.
		self addPathFrom: location1_1_b direction: 'e' to: location1_1_c.
		self addPathFrom: location1_1_c direction: 's' to: location1_2_c.
		self addPathFrom: location1_2_c direction: 'w' to: location1_2_b.

		self addPathFrom: location1_1_a direction: 's' to: location1_2_a.
		self addPathFrom: location1_2_a direction: 's' to: location1_3_a.
		self addPathFrom: location1_3_a direction: 'e' to: location1_3_b.
		self addPathFrom: location1_3_b direction: 's' to: location1_4_b.

		self addPathFrom: location1_4_b direction: 'e' to: location1_4_c.
		self addPathFrom: location1_4_c direction: 'n' to: location1_3_c.
		self addPathFrom: location1_3_c direction: 'e' to: location1_3_d.

		self addPathFrom: location1_4_b direction: 'w' to: location1_4_a.
		self addPathFrom: location1_4_a direction: 's' to: location1_5_a.
		self addPathFrom: location1_5_a direction: 'e' to: location1_5_b.
		self addPathFrom: location1_5_b direction: 'e' to: location1_5_c.
		self addPathFrom: location1_5_c direction: 'e' to: location1_5_d.
		self addPathFrom: location1_5_d direction: 'n' to: location1_4_d.
		self addPathFrom: location1_4_d direction: 'e' to: location1_4_e.

		self addPathFrom: location1_4_e direction: 's' to: location1_5_e.
		self addPathFrom: location1_5_e direction: 'e' to: location1_5_f.

		self addPathFrom: location1_4_e direction: 'n' to: location1_3_e.

		self addPathFrom: location1_3_e direction: 'e' to: location1_3_f.
		self addPathFrom: location1_3_f direction: 's' to: location1_4_f.
		self addPathFrom: location1_4_f direction: 'e' to: location1_4_g.

		"todo self addPathFrom: location1_4_g direction: 'up' to: location2_1_a."

		self addPathFrom: location1_3_e direction: 'n' to: location1_2_e.
		self addPathFrom: location1_2_e direction: 'w' to: location1_2_d.
		self addPathFrom: location1_2_d direction: 'n' to: location1_1_d.
		self addPathFrom: location1_1_d direction: 'e' to: location1_1_e.
		self addPathFrom: location1_1_e direction: 'e' to: location1_1_f.

		self addPathFrom: location1_1_f direction: 's' to: location1_2_f.
		self addPathFrom: location1_2_f direction: 'e' to: location1_2_g.
		self addPathFrom: location1_2_g direction: 's' to: location1_3_g.
		self addPathFrom: location1_3_g direction: 'e' to: location1_3_h.
		self addPathFrom: location1_3_h direction: 'n' to: location1_2_h.

		self addPathFrom: location1_1_f direction: 'e' to: location1_1_g.

		self addPathFrom: location1_1_g direction: 's' to: location1_2_g.

		self addPathFrom: location1_1_g direction: 'e' to: location1_1_h.
		self addPathFrom: location1_1_h direction: 'e' to: location1_1_i.
		self addPathFrom: location1_1_i direction: 'e' to: location1_1_j.
		self addPathFrom: location1_1_j direction: 's' to: location1_2_j.
		self addPathFrom: location1_2_j direction: 'w' to: location1_2_i.
		self addPathFrom: location1_2_i direction: 's' to: location1_3_i.
		self addPathFrom: location1_3_i direction: 's' to: location1_4_i.

		self addPathFrom: location1_4_i direction: 'e' to: location1_4_j.
		self addPathFrom: location1_4_j direction: 'n' to: location1_3_j.

		self addPathFrom: location1_4_i direction: 'w' to: location1_4_h.
		self addPathFrom: location1_4_h direction: 's' to: location1_5_h.

		self addPathFrom: location1_5_h direction: 'w' to: location1_5_g.

		self addPathFrom: location1_5_h direction: 'e' to: location1_5_i.
		self addPathFrom: location1_5_i direction: 'e' to: location1_5_j.
    ]

    initializeSecondFloor [
        | location2_1_a location2_1_b location2_1_c location2_1_d location2_1_e location2_2_a location2_2_b location2_2_c location2_2_d location2_2_e
		location2_3_a location2_3_b location2_3_c location2_3_d location2_3_e location2_4_a location2_4_b location2_4_c location2_4_d location2_4_e
		location2_5_a location2_5_b location2_5_c location2_5_d location2_5_e |

        location2_1_a := Location new: '1_a' floor: 2.
        location2_1_b := Location new: '1_b' floor: 2.
        location2_1_c := Location new: '1_c' floor: 2.
        location2_1_d := Location new: '1_d' floor: 2.
        location2_1_e := Location new: '1_e' floor: 2.
        location2_2_a := Location new: '2_a' floor: 2.
        location2_2_b := Location new: '2_b' floor: 2.
        location2_2_c := Location new: '2_c' floor: 2.
        location2_2_d := Location new: '2_d' floor: 2.
        location2_2_e := Location new: '2_e' floor: 2.
        location2_3_a := Location new: '3_a' floor: 2.
        location2_3_b := Location new: '3_b' floor: 2.
        location2_3_c := Location new: '3_c' floor: 2.
        location2_3_d := Location new: '3_d' floor: 2.
        location2_3_e := Location new: '3_e' floor: 2.
        location2_4_a := Location new: '4_a' floor: 2.
        location2_4_b := Location new: '4_b' floor: 2.
        location2_4_c := Location new: '4_c' floor: 2.
        location2_4_d := Location new: '4_d' floor: 2.
        location2_4_e := Location new: '4_e' floor: 2.
        location2_5_a := Location new: '5_a' floor: 2.
        location2_5_b := Location new: '5_b' floor: 2.
        location2_5_c := Location new: '5_c' floor: 2.
        location2_5_d := Location new: '5_d' floor: 2.
        location2_5_e := Location new: '5_e' floor: 2.

        self addLocation: location2_1_a.
        self addLocation: location2_1_b.
        self addLocation: location2_1_c.
        self addLocation: location2_1_d.
        self addLocation: location2_1_e.
		self addLocation: location2_2_a.
        self addLocation: location2_2_b.
        self addLocation: location2_2_c.
        self addLocation: location2_2_d.
        self addLocation: location2_2_e.
		self addLocation: location2_3_a.
        self addLocation: location2_3_b.
        self addLocation: location2_3_c.
        self addLocation: location2_3_d.
        self addLocation: location2_3_e.
		self addLocation: location2_4_a.
        self addLocation: location2_4_b.
        self addLocation: location2_4_c.
        self addLocation: location2_4_d.
        self addLocation: location2_4_e.
		self addLocation: location2_5_a.
        self addLocation: location2_5_b.
        self addLocation: location2_5_c.
        self addLocation: location2_5_d.
        self addLocation: location2_5_e.

		self addPathFrom: location2_1_a direction: 'e' to: location2_1_b.
		self addPathFrom: location2_1_b direction: 'e' to: location2_1_c.
		self addPathFrom: location2_1_c direction: 's' to: location2_2_c.

		self addPathFrom: location2_2_c direction: 'w' to: location2_2_b.
		self addPathFrom: location2_2_b direction: 's' to: location2_3_b.
		self addPathFrom: location2_3_b direction: 's' to: location2_4_b.

		self addPathFrom: location2_4_b direction: 'e' to: location2_4_c.
		self addPathFrom: location2_4_c direction: 'e' to: location2_4_d.

		self addPathFrom: location2_4_b direction: 's' to: location2_5_b.
		self addPathFrom: location2_5_b direction: 'w' to: location2_5_a.
		self addPathFrom: location2_5_a direction: 'n' to: location2_4_a.
		self addPathFrom: location2_4_a direction: 'n' to: location2_3_a.
		self addPathFrom: location2_3_a direction: 'n' to: location2_2_a.

		self addPathFrom: location2_2_c direction: 's' to: location2_3_c.
		self addPathFrom: location2_3_c direction: 'e' to: location2_3_d.

		self addPathFrom: location2_3_d direction: 'n' to: location2_2_d.
		self addPathFrom: location2_2_d direction: 'n' to: location2_1_d.

		self addPathFrom: location2_3_d direction: 'e' to: location2_3_e.

		self addPathFrom: location2_3_e direction: 'n' to: location2_2_e.
		self addPathFrom: location2_2_e direction: 'n' to: location2_1_e.

		self addPathFrom: location2_3_e direction: 's' to: location2_4_e.
		self addPathFrom: location2_4_e direction: 's' to: location2_5_e.
		self addPathFrom: location2_5_e direction: 'w' to: location2_5_d.
		self addPathFrom: location2_5_d direction: 'w' to: location2_5_c.
	]

    initialize [
        locations := OrderedCollection new.  "Używamy OrderedCollection zamiast Dictionary"
        paths := OrderedCollection new.
        self initializeFirstFloor.
        self initializeSecondFloor.
    ]

    locations [
        ^locations.
    ]

    addLocation: aLocation [
        locations add: aLocation.
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
