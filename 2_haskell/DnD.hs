-- Dungeons and Dziekans
-- Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska
import Data.Char (isSpace)
import Data.List (find, intercalate)
import System.Random (randomRIO)

import GameMap
import Locations
import ItemsNPCs
import IOFunctions
import GameState
import Interactions

instructionsText = [
    "",
    "Dostępne akcje to:",
    "start                  -- Rozpocznij grę.",
    "go n                   -- Idź w kierunku północnym.",
    "go e                   -- Idź w kierunku wschodnim.",
    "go w                   -- Idź w kierunku zachodnim.",
    "go s                   -- Idź w kierunku południowym.",
    "go up                  -- Idź na górę.",
    "go down                -- Idź na dół.",
    "take <item_name>       -- Podnieś przedmiot.",
    "drop <item_name>   -- Upuść przedmiot.",
    "look                   -- Rozejrzyj się.",
    "stats                  -- Wyświetl swoje statystyki.",
    "inventory              -- Wyświetl swoje przedmioty.",
    "instructions           -- Wyświetl instrukcje ponownie.",
    "quit                   -- Zakończ rozgrywkę i wyjdź.",
    "" 
    ]


printInstructions = printLines instructionsText

introductionText = [
    "Witaj w Dungeons and Dziekans. Jesteś studentem, który niedawno obronił pracę inżynierską. Przy odbiorze dyplomu z dziekanatu wyszło na jaw, że nie opłaciłeś warunku. Znajdź dziekana i się z nim rozmów lub ucieknij niepostrzeżenie."
    ]

printIntroduction = printLines introductionText

pathExists :: Location -> String -> [Path] -> Bool
pathExists currLocation dir ps = any (\(from, direction, _) -> from == currLocation && direction == dir) ps

getNewLocation :: Location -> String -> [Path] -> Location
getNewLocation currentLocation dir paths =
    to
    where
        (from, direction, to) = head $ filter (\(from, direction, _) -> from == currentLocation && direction == dir) paths

go :: String -> GameState -> [Path] -> (GameState, String)
go direction gameState paths
    | direction `elem` ["n", "e", "w", "s", "up", "down"] = 
        if pathExists (currentLocation gameState) direction paths
            then let 
                     newLocation = getNewLocation (currentLocation gameState) direction paths
                     message = "Idziesz w kierunku " ++ direction ++ "."
                in (gameState { currentLocation = newLocation }, message)
            else (gameState, "Nie możesz tam iść.")
    | otherwise = (gameState, "podany kierunek nie istnieje")

findItems :: String -> Location -> ItemsLocations -> Maybe Item
findItems name loc itemsMap = 
    fst <$> find (\(item, location) -> itemName item == name && location == loc) itemsMap

availableDirections :: Location -> GameState -> [String]
availableDirections currentLoc gameState =
    [direction | (from, direction, _) <- bidirectionalPaths paths, from == currentLoc]

look :: GameState -> String
look gameState =
    let
        loc = currentLocation gameState
        items = [item | (item, location) <- itemsMap gameState, location == loc]
        directionsDescription = "Dostępne kierunki: " ++ intercalate ", " (availableDirections loc gameState)
        locationDescription = describeLocation loc
        itemDescription = if null items
            then ""
            else "Widzisz następujące przedmioty:\n" ++ intercalate "\n" (map show items) ++ "\n"
    in
        "\n" ++ locationDescription ++ "\n" ++ directionsDescription ++ "\n" ++ itemDescription



takeItem :: String -> GameState -> (GameState, String)
takeItem itemName gameState = 
    let foundItem = findItems itemName (currentLocation gameState) (itemsMap gameState)
    in case foundItem of
        Just existingItem -> 
            let newItemsMap = filter ((/= existingItem) . fst) (itemsMap gameState)
                newInventory = existingItem : inventory gameState
                message = "Podnosisz przedmiot: " ++ show existingItem ++ "\n"
                newGameState = updateStatsAfterTake gameState existingItem
            in (newGameState { itemsMap = newItemsMap, inventory = newInventory }, message)
        Nothing -> 
            (gameState, "Nie ma takiego przedmiotu w tej lokalizacji.\n")

seeInventory :: GameState -> String
seeInventory gameState
    | null (inventory gameState) = "Nie masz żadnych przedmiotów.\n"
    | otherwise = "Twoje przedmioty:\n" ++ intercalate "\n" (map show (inventory gameState)) ++ "\n"

updateStatsAfterLeave :: GameState -> Item -> GameState
updateStatsAfterLeave gameState foundItem = 
    case itemStat foundItem of
        Just "Attack" -> 
            let currAttack = herosAttack (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosAttack = currAttack - itemValue foundItem } }
            in newGameState
        
        Just "Defense" -> 
            let currDefense = herosDefense (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosDefense = currDefense - itemValue foundItem } }
            in newGameState
        
        Just "Energy" -> 
            let currEnergy = herosEnergy (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosEnergy = currEnergy - itemValue foundItem } }
            in newGameState

        Just "Charisma" -> 
            let currCharisma = herosCharisma (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosCharisma = currCharisma - itemValue foundItem } }
            in newGameState

        _ -> gameState

findItemInInventory :: String -> [Item] -> Maybe Item
findItemInInventory name items = find (\item -> itemName item == name) items

leaveItem :: String -> GameState -> (GameState, String)
leaveItem itemName gameState =
    let foundItem = findItemInInventory itemName (inventory gameState)

    in case foundItem of
        Just existingItem -> 
            let newInventory = filter (/= existingItem) (inventory gameState)
                newItemsMap = (existingItem, currentLocation gameState) : itemsMap gameState
                newGameState = updateStatsAfterLeave gameState existingItem
                message = "Zostawiasz przedmiot: " ++ show existingItem ++ "\n"
            in (newGameState { itemsMap = newItemsMap, inventory = newInventory }, message)

        Nothing -> 
            (gameState, "Nie masz takiego przedmiotu w ekwipunku.\n")

formatStats :: Stats -> String
formatStats stats = "\nEnergia - " ++ show (herosEnergy stats) ++
        "\nAtak - " ++ show (herosAttack stats) ++ 
        "\nObrona - " ++ show (herosDefense stats) ++ 
        "\nCharyzma - " ++ show (herosCharisma stats) ++ "\n"


interactionsLoop :: GameState -> [Path] -> IO ()
interactionsLoop gameState paths =
    if (running gameState) then do 
        let npc = fst $ head $ filter (\(_, npcLocation) -> npcLocation == currentLocation gameState) (npcsMap gameState)

        newGameState <- case npcName npc of
            "Wykładowcę" -> interactWithLecturer gameState
            "Automat" -> interactWithVendingMachine gameState
            "Dozorcę" -> interactWithJanitor gameState
            "Dziekana" -> interactWithDean gameState
            _ -> return gameState
        
        gameLoop newGameState paths
    else
        return ()


gameLoop :: GameState -> [Path] -> IO ()
gameLoop gameState paths = do
    if (running gameState) then do 
        cmd <- readCommand
        let (action, argument) = parseCommand cmd
        case action of
            "go" -> do
                let (newGameState, message) = go argument gameState paths
                printLines [message, look newGameState]
                if checkForNPC (currentLocation newGameState) (npcsMap newGameState) then do
                    printLines [encounterNPC (currentLocation newGameState) (npcsMap newGameState)]
                    interactionsLoop newGameState paths
                else 
                    gameLoop newGameState paths
            "take" -> do
                let (newGameState, message) = takeItem argument gameState
                printLines [message]
                gameLoop newGameState paths
            "drop" -> do
                let (newGameState, message) = leaveItem argument gameState
                printLines [message]
                gameLoop newGameState paths
            "look" -> do
                printLines [look (gameState)]
                gameLoop gameState paths
            "stats" -> do
                printLines ["Twoje statystyki:", formatStats (stats gameState)]
                gameLoop gameState paths
            "inventory" -> do
                printLines [seeInventory (gameState)]
                gameLoop gameState paths
            "instructions" -> do
                printInstructions
                gameLoop gameState paths
            "quit" -> return ()
            _ -> do
                printLines ["Nieznana komenda. Wyświetl instrukcje używając 'instructions'.", ""]
                gameLoop gameState paths
    else do 
        return ()


createPlayer :: IO Stats
createPlayer = do
    charisma <- randomRIO (0 :: Int, 1 :: Int)
    attack <- randomRIO (3 :: Int, 8 :: Int)
    defense <- randomRIO (0 :: Int, 5 :: Int)
    energy <- randomRIO (15 :: Int, 25 :: Int)

    let stats = Stats energy attack defense charisma
    return stats


main :: IO ()
main = do
    printIntroduction
    printInstructions
    (updatedItems, updatedNPCs) <- chooseRandomLocations initialItems initialNPCs
    stats <- createPlayer
    let initialGameState = GameState location1_1_a [] updatedItems updatedNPCs stats False True
    printLines ["", look initialGameState]
    gameLoop initialGameState (bidirectionalPaths paths)