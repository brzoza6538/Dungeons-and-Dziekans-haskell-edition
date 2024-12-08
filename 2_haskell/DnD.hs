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
    "throwout <item_name>   -- odrzuć przedmiot.",
    "look                   -- Rozejrzyj się.",
    "stats                  -- Wyświetl swoje statystyki.",
    "inventory              -- Wyświetl swoje przedmioty.",
    "instructions           -- Wyświetl instrukcje ponownie.",
    "quit                   -- Zakończ rozgrywkę i wyjdź.",
    "" 
    ]


interactionInstructionsText = [
    "",
    "Dostępne akcje to:",

    "interact           -- porozmawiaj/użyj",
    "argue              -- 'zaatakuj'.",
    "leave              -- opuść walkę",

    "stats              -- Wyświetl swoje statystyki.",
    "inventory          -- Wyświetl swoje przedmioty.",
    "instructions       -- Wyświetl instrukcje ponownie.",
    "quit               -- Zakończ rozgrywkę i wyjdź.",
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
                     npcMessage = encounterNPC newLocation (npcsMap gameState)
                     message = "Idziesz w kierunku " ++ direction ++ ".\n" ++ describeLocation newLocation ++ "\n" ++ npcMessage
                in (gameState { currentLocation = newLocation }, message)
            else (gameState, "Nie możesz tam iść.")
    | otherwise = (gameState, "podany kierunek nie istnieje")

findItems :: String -> Location -> ItemsLocations -> Maybe Item
findItems name loc itemsMap = 
    fst <$> find (\(item, location) -> itemName item == name && location == loc) itemsMap

look :: GameState -> String
look gameState = do
    let loc = currentLocation gameState
        items = [item | (item, location) <- itemsMap gameState, location == loc]
    if null items
        then "Nie widzisz żadnych przedmiotów."
        else "Widzisz następujące przedmioty:\n" ++ intercalate "\n" (map show items)



takeItem :: String -> GameState -> (GameState, String)
takeItem itemName gameState = 
    let foundItem = findItems itemName (currentLocation gameState) (itemsMap gameState)
    in case foundItem of
        Just existingItem -> 
            let newItemsMap = filter ((/= existingItem) . fst) (itemsMap gameState)
                newInventory = existingItem : inventory gameState
                message = "Podnosisz przedmiot: " ++ show existingItem
                newGameState = updateStatsAfterTake gameState existingItem
            in (newGameState { itemsMap = newItemsMap, inventory = newInventory }, message)
        Nothing -> 
            (gameState, "Nie ma takiego przedmiotu w tej lokalizacji.")

seeInventory :: GameState -> String
seeInventory gameState
    | null (inventory gameState) = "Nie masz żadnych przedmiotów."
    | otherwise = "Twoje przedmioty:\n" ++ intercalate "\n" (map show (inventory gameState))

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
                message = "Zostawiasz przedmiot: " ++ show existingItem
            in (newGameState { itemsMap = newItemsMap, inventory = newInventory }, message)

        Nothing -> 
            (gameState, "Nie masz takiego przedmiotu w ekwipunku.")

interactionsLoop :: GameState -> [Path] -> IO ()
interactionsLoop gameState paths =
    if (running gameState) then do 
        let npc = fst $ head $ filter (\(_, npcLocation) -> npcLocation == currentLocation gameState) (npcsMap gameState)

        cmd <- readCommand
        let (action, argument) = parseCommand cmd
        case action of
            "interact" -> do
                if uncooperative npc then do 
                    printLines ["To się chyba nie uda..."]
                    interactionsLoop gameState paths
                else do
                    newGameState <- talkWithNPC gameState npc
                    interactionsLoop newGameState paths

            "argue" -> do
                newGameState <- attackNPC gameState
                gameLoop newGameState paths


            "leave" -> do
                if (uncooperative npc && npcName npc /= "Automat") then do 
                    printLines [npcName npc ++ " nie wydaje się chcieć cię przepuścić"]
                    interactionsLoop gameState paths
                else do
                    printLines ["Omijasz " ++ npcName npc ++ " i idziesz dalej"]
                    gameLoop gameState paths


            "stats" -> do
                printLines ["Twoje statystyki:", show (stats gameState)]
                interactionsLoop gameState paths

            "inventory" -> do
                printLines [seeInventory (gameState)]
                interactionsLoop gameState paths
            

            "instructions" -> do
                printLines interactionInstructionsText
                interactionsLoop gameState paths

            "quit" -> do 
                return ()

            _ -> do
                printLines ["Nieznana komenda. Wyświetl instrukcje używając 'instructions'.", ""]
                interactionsLoop gameState paths
    else do 
        return ()

gameLoop :: GameState -> [Path] -> IO ()
gameLoop gameState paths = do
    if (running gameState) then do 

        cmd <- readCommand
        let (action, argument) = parseCommand cmd
        -- printGameState gameState
        case action of
            "go" -> do
                let (newGameState, message) = go argument gameState paths
                printLines [message]
                if checkForNPC (currentLocation newGameState) (npcsMap newGameState) then 
                    interactionsLoop newGameState paths
                else 
                    gameLoop newGameState paths
            "take" -> do
                let (newGameState, message) = takeItem argument gameState
                printLines [message]
                gameLoop newGameState paths
            
            "throwout" -> do
                let (newGameState, message) = leaveItem argument gameState
                printLines [message]
                gameLoop newGameState paths

            "look" -> do
                printLines [look (gameState)]
                gameLoop gameState paths
            "stats" -> do
                printLines ["Twoje statystyki:", show (stats gameState)]
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


printGameState :: GameState -> IO ()
printGameState gameState = do
    printLines ["Items:"]
    printLines (map show (itemsMap gameState))
    printLines ["", "nNPCs:"]
    printLines (map show (itemsMap gameState))


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
    gameLoop (GameState location1_1_a [] updatedItems updatedNPCs stats True) (bidirectionalPaths paths)