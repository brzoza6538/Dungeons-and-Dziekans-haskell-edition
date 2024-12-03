-- Dungeons and Dziekans
-- Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska
import Data.Char (isSpace)

import GameMap

data GameState = GameState { currentLocation :: Location }

--  utils
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

parseCommand :: String -> (String, String)
parseCommand input =
    let (cmd:args) = words input ++ [""]
        trimmedArgs = dropWhile isSpace (reverse (dropWhile isSpace (reverse (unwords args))))
    in (cmd, trimmedArgs)

instructionsText = [
    "",
    "Dostępne akcje to:",
    "start              -- Rozpocznij grę.",
    "go n               -- Idź w kierunku północnym.",
    "go e               -- Idź w kierunku wschodnim.",
    "go w               -- Idź w kierunku zachodnim.",
    "go s               -- Idź w kierunku południowym.",
    "go up              -- Idź na górę.",
    "go down            -- Idź na dół.",
    "take <item_id>     -- Podnieś przedmiot.",
    "look               -- Rozejrzyj się.",
    "stats              -- Wyświetl swoje statystyki.",
    "items              -- Wyświetl swoje przedmioty.",
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
    to  -- Bezpośrednio zwracamy 'to', które jest w pasującej ścieżce
    where
        (from, direction, to) = head $ filter (\(from, direction, _) -> from == currentLocation && direction == dir) paths

go :: String -> GameState -> [Path] -> IO GameState
go direction gameState paths
    | direction `elem` ["n", "e", "w", "s", "up", "down"] = do
        if pathExists (currentLocation gameState) direction paths
            then do
                let newLocation = getNewLocation (currentLocation gameState) direction paths
                putStrLn $ "Idziesz w kierunku " ++ direction ++ "."
                return gameState { currentLocation = newLocation }
            else do
                putStrLn "Nie możesz tam iść."
                return gameState
    | otherwise = do
        putStrLn "Nie możesz tam iść."
        return gameState

takeItem :: String -> IO ()
takeItem item
    | null item = do
        putStrLn "Nie podałeś ID przedmiotu do podniesienia."
    | otherwise = do
        putStrLn $ "Podnosisz przedmiot: " ++ item ++ ". [NIE ZAIMPLEMENTOWANO]"

gameLoop :: GameState -> [Path] -> IO ()
gameLoop gameState paths = do
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "go" -> do
            newGameState <- go argument gameState paths
            gameLoop newGameState paths
        "take" -> do
            takeItem argument
            gameLoop gameState paths
        "look" -> do
            printLines ["Rozglądasz się", "[NIE ZAIMPLEMENTOWANO]", ""]
            gameLoop gameState paths
        "stats" -> do
            printLines ["Twoje statystyki:", "[NIE ZAIMPLEMENTOWANO]", ""]
            gameLoop gameState paths
        "items" -> do
            printLines ["Twoje przedmioty:", "[NIE ZAIMPLEMENTOWANO]", ""]
            gameLoop gameState paths
        "instructions" -> do
            printInstructions
            gameLoop gameState paths
        "quit" -> return ()  -- Zakończenie gry, ale bez zmiany stanu gry
        _ -> do
            printLines ["Nieznana komenda.", ""]
            gameLoop gameState paths

main :: IO ()
main = do
    printIntroduction
    printInstructions
    let initialLocation = Location 1 "start"
    let initialState = GameState { currentLocation = initialLocation }
    gameLoop initialState paths
