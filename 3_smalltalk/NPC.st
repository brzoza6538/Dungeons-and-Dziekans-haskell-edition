Object subclass: NPC [
    | npcName energy attack defense inventory |

    NPC class >> new: name energy: anEnergy attack: anAttack defense: aDefense inventory: anInventory [
        ^self new 
            npcName: name;
            energy: anEnergy;
            attack: anAttack;
            defense: aDefense;
            inventory: anInventory.
    ]

    npcName: aName [
        npcName := aName.
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

    npcName [
        ^npcName.
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
