module Interactions where

import IOFunctions
import System.Random (randomRIO)
import GameState
import ItemsNPCs
import GameMap

updateStatsAfterTake :: GameState -> Item -> GameState
updateStatsAfterTake gameState foundItem = 
    case itemStat foundItem of
        Just "Attack" -> 
            let currAttack = herosAttack (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosAttack = currAttack + itemValue foundItem } }
            in newGameState
        
        Just "Defense" -> 
            let currDefense = herosDefense (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosDefense = currDefense + itemValue foundItem } }
            in newGameState
        
        Just "Energy" -> 
            let currEnergy = herosEnergy (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosEnergy = currEnergy + itemValue foundItem } }
            in newGameState

        Just "Charisma" -> 
            let currCharisma = herosCharisma (stats gameState)
                newGameState = gameState { stats = (stats gameState) { herosCharisma = currCharisma + itemValue foundItem } }
            in newGameState

        _ -> gameState


itemFromMachine :: GameState -> IO (GameState, String)
itemFromMachine gameState = do
    idx <- randomRIO (0, length vendingMachineInventory - 1)
    let 
        item = vendingMachineInventory !! idx
        newInventory = item : (inventory gameState)
        newGameState = updateStatsAfterTake gameState item 

    return (newGameState { inventory = newInventory }, "Otrzymujesz: " ++ (itemName item))


interactWithLecturer :: GameState -> IO GameState
interactWithLecturer gameState = do
    printLines ["Witasz wykładowcę, który zaczyna mówić o twoich problemach w nauce",
                "1) starasz się wytłumaczyć",
                "2) obrażony wyzywasz go",
                "3) próbujesz go zmanipulować by ci pomógł", ""]
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "1" -> do
            printLines ["Wykładowca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie", ""]
            interactWithLecturer gameState

        "2" -> do
            printLines ["Wykładowca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: 'Dosć padło słów, czas podyskutować'", ""]
            attackNPC gameState

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                printLines ["Dzięki odrobinie charyzmy wykadowca zdradza ci sekret. Przed wyjściem powinien właśnie teraz stać dozorca. Jest to rozsądny człowiek, może uda ci się z nim dogadać", ""]
                let newMap = addJanitor (npcsMap gameState)
                let updatedNPCLocations = filter (\(npc', _) -> npcName npc' /= "Wykładowcę") newMap
                let newGameState = gameState { npcsMap = updatedNPCLocations }
                return newGameState
            else do  
                printLines ["Akurat kiedy myślałeś że wykładowca za chwilę powie coś ważnego, nagle twarz mu pochmurnieje. Zauważył co próbowałeś zrobić i nie jest zadowolony.", ""]
                attackNPC gameState

        _ -> do 
            printLines ["Wykładowca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?", ""]
            interactWithLecturer gameState


interactWithVendingMachine :: GameState -> IO GameState
interactWithVendingMachine gameState = do
    if vendingMachineUsed gameState then do
        printLines ["Znajduje się tu automat, jednak wydaje się, że nie działa."]
        return gameState
    else do
        printLines ["Automat buczy nieznacznie, czy masz na coś ochotę?", 
                    "1) potrzebuję się napić", 
                    "2) przydałyby mi się jakieś lepsze notatki", 
                    "3) chyba nic nie potrzebuję",
                    "4) uderz w automat, może się uda wyciągnąć kilka rzeczy", ""]
        cmd <- readCommand
        let (action, _) = parseCommand cmd
        case action of
            "1" -> itemAction gameState
            "2" -> itemAction gameState
            "3" -> do
                printLines ["Może rzeczywiście to nie czas na zakupy.", ""]
                return gameState
            "4" -> do
                attackNPC gameState
            _ -> do
                printLines ["Automat wydaje się nie rozumieć twojego wyboru, może spróbujesz jeszcze raz?", ""]
                interactWithVendingMachine gameState
  where
    itemAction :: GameState -> IO GameState
    itemAction gs = do
        (newGameState, message) <- itemFromMachine gs
        printLines ["Wpisujesz kod, jednak automat wydaje się nie przejmować zupełnie twoimi instrukcjami, podając ci coś innego. Wraz z wyrzuceniem przedmiotu, światełko w automacie zaczyna intensywnie migać, aż w końcu gaśnie.", "", message, ""]
        let updatedGameState = newGameState { vendingMachineUsed = True } 
        return updatedGameState
        

      
interactWithJanitor :: GameState -> IO GameState
interactWithJanitor gameState = do
    printLines ["Witasz Dozorcę, jednak ten wydaje się ciebie nie słuchać",
                "1) tłumaczysz swoją sytuację",
                "2) obrażony każesz mu otworzyć drzwi w mniej niż uprzejmy sposób",
                "3) próbujesz ponownie wykorzystać swoją charyzmę", ""]
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "1" -> do
            printLines ["Dozorca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie.", ""]
            interactWithJanitor gameState

        "2" -> do
            printLines ["Dozorca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padlo słów, czas podyskutować", ""]
            attackNPC gameState
        
        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                printLines ["Odrobina uprzejmości i dozorca od razu się do ciebie otwiera. Słuchasz jego historii jak sam był kiedyś studentem, ale warunki zniszczyły mu życie. Po rozmowie postanawia się cie wypuścić. Udało ci się uciec, ale nadal nie rozwiązałeś swoich problemów", "Koniec gry, przeżyłeś dzisiejszy dzień, ale jest to życie pełne strachu i niepewności. Czy to jest to czego chciałeś?", "", ""]
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                printLines ["Jak się okazuje, dozorca nie ma najmniejszej ochoty z tobą rozmawiać, a twoje natręctwo tylko go zirytowało.", ""]
                attackNPC gameState
        _ -> do 
            printLines ["Dozorca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?", ""]
            interactWithJanitor gameState



interactWithDean :: GameState -> IO GameState
interactWithDean gameState = do 
    printLines ["Witasz Dziekana, jednak ten nie wydaje się chętny do słuchania wymówek",
                "1) tłumaczysz swoją sytuację",
                "2) obrażony nonszalancją dziekana wybuchasz i mówisz dwa razy więcej niż powinieneś",
                "3) próbujesz wykorzystać swój urok osobisty",
                "4) mówisz dziekanowi że masz ze sobą legendarny złoty strzał", ""]
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "1" -> do
            printLines ["Dziekan potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie.", ""]
            interactWithDean gameState

        "2" -> do
            printLines ["Dziekan wydaje się obrażony, możliwe że powiedziałeś za dużo.", ""]
            attackNPC gameState
        
        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 1) then do
                printLines ["Dziekan słucha cię uważnie, twoje idealnie dobrane odzienie, naturalna charyzma i umniejętność zmyślania historii w czasie rzeczywistym, sprawiają że nie może ciebie nie wysłuchać. Po chwili namysłu postanawia ci pomóc. Możesz uznać swój dług za spłacony", "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że przyszłość jest w twoich rękach.", "", ""]
                let newGameState = gameState { running = False } 
                return newGameState
            else do  
                printLines ["Dziekan wydaje się słuchać ciebie z uwagą, jednak w połowie twojej historii o tragedii jak twój trzeci pies zjadł również twój laptop podczas pogrzebu twojej babci, zaczyna się z ciebie śmiać. Nie rozwiążesz tego rozmową.", ""]
                attackNPC gameState

        "4" -> do
            let strzalCheck = any (\item -> itemName item == "Złoty strzał") (inventory gameState)
            if strzalCheck then do
                printLines ["Dziekan stoi przez chwilę oniemiały, był pewny że to tylko legenda. Jednak wie dobrze że jest to coś co musi uhonorować", "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że masz przyszłość.", "", ""]
                let newGameState = gameState { running = False } 
                return newGameState
                
            else do
                printLines ["Dziekan stoi przez chwilę oniemiały, był pewny śmieje się z ciebie i pyta czy to też pies ci zjadł", ""]
                interactWithDean gameState

        _ -> do  
            printLines ["Dziekan nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?", ""]
            interactWithDean gameState

updateNPC :: GameState -> Int -> GameState
updateNPC gameState damage = 
    let npc = fst $ head $ filter (\(_, npcLocation) -> npcLocation == currentLocation gameState) (npcsMap gameState)
        currEnergy = (energy npc) - damage
    in if currEnergy <= 0 
       then 
            let updatedNPCLocations = filter (\(npc', _) -> npc' /= npc) (npcsMap gameState)
            in gameState { npcsMap = updatedNPCLocations }
       else 
            let updatedNPC = npc { energy = currEnergy }
                updatedNPCLocations = map (\(npc', loc) -> if npc' == npc then (updatedNPC, loc) else (npc', loc)) (npcsMap gameState)
            in gameState { npcsMap = updatedNPCLocations }


updateHero :: GameState -> Int -> GameState
updateHero gameState damage = 
    let currEnergy = (herosEnergy (stats gameState)) - damage
    in (gameState { stats = (stats gameState) { herosEnergy = currEnergy } })


attackNPC :: GameState -> IO GameState
attackNPC gameState = do
  let npc = fst $ head $ filter (\(_, npcLocation) -> npcLocation == currentLocation gameState) (npcsMap gameState)
  
  case npcName npc of
    "Automat" -> do 
        printLines ["Maszyna wydaje się buczyć 'Ach, wreszcie godny oponent...'", "Po czym wybucha drastycznie ciebie raniąc. Sfrustrowany dziekan podziwia twoje poświęcenie oglądając jak twoje nieprzytomne ciało jest wywożone z wydziału ambulansem bez uiszczenia opłaty", "Koniec gry. Nie musisz się już martwić warunkami, właściwie nie martwisz się już o nic..."]
        let newGameState = gameState { running = False } 
        return newGameState
        
    _ -> do 
        -- +/- 2 do ataku bazowego minus defense przeciwnika 
        randOffset <- randomRIO ((0) :: Int, (3) :: Int) 
        let
            damageNPC = (herosAttack (stats gameState)) + randOffset -  (defense npc)
            halfwayGameState = updateNPC gameState damageNPC
        printLines ["Zadajesz " ++ show damageNPC ++ " obrażenia"]

        randOffset <- randomRIO ((0) :: Int, (3) :: Int) 
        let
            damageHero = (attack npc) + randOffset -  (herosDefense (stats gameState))
            finalGameState = updateHero halfwayGameState damageHero
        printLines [npcName npc ++ " zadaje ci " ++ show damageHero ++ " obrażeń"]


        if ((herosEnergy (stats gameState)) - damageHero < 0) then do
                printLines ["Koniec gry. Zostałeś pokonany"]
                let endGameState = finalGameState { running = False } 
                return endGameState

        else if ((energy npc) - damageNPC <= 0) then do
            if npcName npc == npcName janitor then do 
                printLines ["Dozorca został pokonany, nic nie stoi ci na drodze do wyjścia. Ale nadal czujesz ogromny ciężar na sercu, wiesz że to jeszcze nie koniec...", "Koniec gry."]
                let endGameState = finalGameState { running = False } 
                return endGameState

            else do
                printLines ["I jak stał. " ++ npcName npc ++ " teraz już nie stoi"]
                return finalGameState 

        else do 
            printLines ["Przeciwnikowi zostało " ++ show ((energy npc) - damageNPC) ++ " energii.", "Zostało ci " ++ show (herosEnergy (stats finalGameState)) ++ " energii.", ""]
            attackNPC finalGameState

