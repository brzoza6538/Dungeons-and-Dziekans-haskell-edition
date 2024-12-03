-- Dungeons and Dziekans
-- Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska
import Data.Char (isSpace)

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


go :: String -> IO ()
go direction
    | direction `elem` ["n", "e", "w", "s", "up", "down"] = do
        putStrLn $ "Idziesz w kierunku " ++ direction ++ ". [NIE ZAIMPLEMENTOWANO]"
    | otherwise = do
        putStrLn "Nieznany kierunek."


takeItem :: String -> IO ()
takeItem item
    | null item = do
        putStrLn "Nie podałeś ID przedmiotu do podniesienia."
    | otherwise = do
        putStrLn $ "Podnosisz przedmiot: " ++ item ++ ". [NIE ZAIMPLEMENTOWANO]"


-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "go" ->     do go argument
                       gameLoop
        "take" ->   do takeItem argument
                       gameLoop
        "look" ->   do printLines ["Rozglądasz się", "[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "stats" ->  do printLines ["Twoje statystyki:", "[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "items" ->  do printLines ["Twoje przedmioty:", "[NIE ZAIMPLEMENTOWANO]", ""]
        "instructions" -> do printInstructions
                             gameLoop
        "quit" -> return ()
        _ -> do printLines ["Nieznana komenda.", ""]
                gameLoop


main = do
    printIntroduction
    printInstructions
    gameLoop