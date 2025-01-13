Object subclass: Player [
    | name energy attack defense inventory |

    Player class >> new: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory [
        ^self new initialize: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory.
    ]

    initialize: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory [
        self name: name;
            energy: anEnergy;
            attack: anAttack;
            defense: aDefense;
            inventory: anInventory.
    ]

    name: aName [
        name := aName.
    ]

    energy: anEnergy [
        energy := anEnergy.
    ]

    attack: anAttack [
        attack := anAttack.
    ]

    defense: aDefense [
        defense := aDefense.
    ]

    inventory: anInventory [
        inventory := anInventory.
    ]

    name [
        ^name.
    ]

    energy [
        ^energy.
    ]

    attack [
        ^attack.
    ]

    defense [
        ^defense.
    ]

    inventory [
        ^inventory.
    ]
]

Player subclass: HumanPlayer [
    | charisma |

    HumanPlayer class >> new: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory charisma: aCharisma [
        ^self new initialize: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory charisma: aCharisma.
    ]

    initialize: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory charisma: aCharisma [
        super initialize: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory.
        self charisma: aCharisma.
    ]

    charisma: aCharisma [
        charisma := aCharisma.
    ]

    charisma [
        ^charisma.
    ]

    stats [
        ^'Twoje statystyki: ', '
Energia: ', energy asString, '
Siła: ', attack asString, ' 
Obrona: ', defense asString, '
Charyzma: ', charisma asString, '
'
    ]
]