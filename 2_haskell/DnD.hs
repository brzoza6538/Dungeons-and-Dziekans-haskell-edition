-- Dungeons and Dziekans
-- Jakub Bąba, Michał Brzeziński, Aleksandra Szymańska


--  utils
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs


instructionsText = [
    "",
    "Dostępne akcje to:",
    "start              -- Rozpocznij grę.",
    "n                  -- Idź w kierunku północnym.",
    "e                  -- Idź w kierunku wschodnim.",
    "w                  -- Idź w kierunku zachodnim.",
    "s                  -- Idź w kierunku południowym.",
    "up                 -- Idź na górę.",
    "down               -- Idź na dół.",
    "take(id)           -- Podnieś przedmiot.",
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


-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    case cmd of
        "n" ->      do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "e" ->      do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "w" ->      do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "s" ->      do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "up" ->     do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "down" ->   do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "take" ->   do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "look" ->   do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "stats" ->  do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
                       gameLoop
        "items" ->  do printLines ["[NIE ZAIMPLEMENTOWANO]", ""]
        "instructions" -> do printInstructions
                             gameLoop
        "quit" -> return ()
        _ -> do printLines ["Nieznana komenda.", ""]
                gameLoop


main = do
    printIntroduction
    printInstructions
    gameLoop