module ItemsNPCs where

data Item = Item
    { itemName :: String
    , itemDescription :: String
    , itemStat :: Maybe String
    , itemValue :: Int
    }
  deriving (Eq)

instance Show Item where
    show item = itemName item ++ " - " ++ itemDescription item



poor_notes = Item "Slabe notatki" "Notatki pozostawione przez studenta pierwszego roku. Nie dają dużo, ale lepszy rydz niż nic. (+2 ataku)" (Just "Attack") 2

outdated_regulations = Item "Nieaktualny regulamin studiów" "Zbiór kartek, na których widnieje wielka liczba '2005'. Może coś jest jeszcze aktualne? (+2 obrony)" (Just "Defense") 2

jackpot = Item "Zloty strzal" "Mówi się o tym jak o legendzie, podobno posiadacz złotego strzału może za darmo anulować wszystkie warunki." Nothing 0

better_notes = Item "Lepsze notatki" "Notatki pozostawione przez studenta pierwszego roku, ale studiów magisterskich, zmagającego się z tym samym problemem umożenia warunku (+3 ataku)." (Just "Attack") 3

up_to_date_regulations = Item "Nowy regulamin" "Zbiór zapisów, którymi możesz się osłonić przed słownymi ciosami pracowników uczelni (+3 obrony)" (Just "Defense") 3

formal_wear = Item "Toga" "Modny dodatek. Ludzie będą traktować cię poważnie, nawet jeśli nie masz nic mądrego do powiedzenia (+1 charyzmy)" (Just "Charisma") 1

energy_drink = Item "Energetyk" "Niezastąpiony w momentach kryzysowych. Daje energię, np.: by dokończyć noc przed oddaniem (prawie) każdy projekt" (Just "Energy") 10



data NPC = NPC
  { npcName :: String
  , energy :: Int
  , attack :: Int
  , defense :: Int
  , uncooperative :: Bool
  }
  deriving (Eq, Show)


dean = NPC "Dziekana" 50 10 4 False

lecturer = NPC "Wykładowcę" 20 5 0 False

janitor = NPC "Dozorcę" 20 3 3 False

vendingMachine = NPC "Automat" 0 0 0 False


vendingMachineInventory :: [Item]
vendingMachineInventory = 
  [
    up_to_date_regulations,
    formal_wear,
    energy_drink,
    better_notes
  ]
  