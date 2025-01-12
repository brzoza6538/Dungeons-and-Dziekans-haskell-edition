Object subclass: Item [
    | itemName itemDescription itemStat itemValue |

    Item class >> new: name description: description stat: stat value: value [
        ^self new 
            itemName: name;
            itemDescription: description;
            itemStat: stat;
            itemValue: value.
    ]

    itemName: aName [
        itemName := aName.
    ]

    itemDescription: aDescription [
        itemDescription := aDescription.
    ]

    itemStat: aStat [
        itemStat := aStat.
    ]

    itemValue: aValue [
        itemValue := aValue.
    ]

    itemName [
        ^itemName.
    ]

    itemDescription [
        ^itemDescription.
    ]

    itemStat [
        ^itemStat.
    ]

    itemValue [
        ^itemValue.
    ]
]
