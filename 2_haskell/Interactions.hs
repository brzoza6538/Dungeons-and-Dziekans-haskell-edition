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


makeUncooperative :: GameState -> NPC-> GameState
makeUncooperative gameState npc =
    let updatedNPC = npc { uncooperative = True }
        updatedNPCLocations =
            map (\(npc', loc) -> if npc' == npc then (updatedNPC, loc) else (npc', loc)) (npcsMap gameState)

    in gameState { npcsMap = updatedNPCLocations }


itemFromMachine :: GameState -> IO GameState
itemFromMachine gameState = do
    idx <- randomRIO (0, length vendingMachineInventory - 1)
    let 
        item = vendingMachineInventory !! idx
        newInventory = item : (inventory gameState)
        newGameState = updateStatsAfterTake gameState item 

    putStrLn ("otrzymujesz: " ++ (itemName item))
    return (newGameState { inventory = newInventory })


talkWithNPC :: GameState -> NPC -> IO GameState
talkWithNPC gameState npc = 
  case npcName npc of
    "Wykładowcę" -> do 
      printLines ["witasz wykładowcę, który zaczyna mówić o twoich problemach w nauce", "1) starasz się wytłumaczyć", "2) obrażony zwyzywasz go", "3) próbujesz go zmanipulować by ci pomógł"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            putStrLn "wykładowca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie"
            return gameState

        "2" -> do
            putStrLn "wykładowca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padlo słów, czas podyskutować"
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                putStrLn "Dzięki odrobinie charyzmy wykadowca zdradza ci sekret. Przed wyjściem powinien właśnie teraz stać dozorca. Jest to rozsądny człowiek może uda ci się z nim dogadać"
                let 
                    newMap = addJanitor (npcsMap gameState)
                    newGameState = gameState {npcsMap = newMap}
                return newGameState
            else do  
                putStrLn "Akurat kiedy myślałeś że wykładowca za chwilę powie coś ważnego, nagle twarz mu pochmurnieje. Zauważył co próbujesz zrobić i nie jest zadowolony"
                let newGameState = makeUncooperative gameState npc
                return newGameState

        _ -> do 
            putStrLn "wykładowca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi"
            return gameState 


    "Automat" -> do 
      printLines ["automat buczy nieznacznie, czy masz na coś ochotę?", "1) potrzebuję się napić", "2) przydałyby mi się jakieś lepsze notatki", "3) chyba nic nie potrzebuję"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "3" -> do
            putStrLn "chyba rzeczywiście to nie czas na zakupy"
            return gameState
        _ -> do 
            putStrLn "wpisujesz do terminalu kod, jednak automat wydaje się nie przejmować zupełnie twoimi sugestiami, podając ci coś innego"
            newGameState <- itemFromMachine gameState

            return (makeUncooperative newGameState npc)
  


    "Dozorcę" -> do 
      printLines ["witasz Dozorcę, jednak ten wydaje się ciebie nie słuchać", "1) tłumaczysz swoją sytuację", "2) obrażony każesz mu otworzyć drzwi w mniej niż uprzejmy sposób", "3) próbujesz ponownie wykorzystać swoją charyzmę"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            putStrLn "Dozorca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie"
            return gameState

        "2" -> do
            putStrLn "Dozorca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padlo słów, czas podyskutować"
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                putStrLn "Odrobina uprzejmości i dozorca od razu się do ciebie otwiera. Słuchasz jego historii jak sam był kiedyś studentem, ale warunki zniszczyły mu życie. Po rozmowie postanawia się cie wypuścić. Udało ci się uciec, ale nadal nie rozwiązałeś swoich problemów"
                putStrLn "Koniec gry, przeżyłeś dzisiejszy dzień, ale jest to życie pełne strachu i niepewności"
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                putStrLn "Jak się okazuje dozorca nie ma najmniejszej ochoty z tobą rozmawiać a twoje natręctwo tylko go zirytowało"
                let newGameState = makeUncooperative gameState npc
                return newGameState

        _ -> do 
            putStrLn "Dozorca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi"
            return gameState 



    "Dziekana" -> do 
      printLines ["witasz Dziekana, jednak ten nie wydaje się chętny do słuchania wymówek", "1) tłumaczysz swoją sytuację", "2) obrażony nonszalancją dziekana wybuchasz i mówisz dwa razy więcej niż powinieneś", "3) próbujesz ponownie wykorzystać swój urok osobisty", "4) mówisz dziekanowi że masz ze sobą legendarny złoty strzał "]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            putStrLn "Dziekan potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie"
            return gameState

        "2" -> do
            putStrLn "Dziekan wydaje się obrażony, możliwe że powiedziałeś za dużo"
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 2) then do
                putStrLn "Dziekan słucha cię uważnie, twoje idealnie dobrane odzienie, naturalna charyzma i umniejętność zmyślania historii w czasie rzeczywistym, sprawiają że nie może ciebie nie wysłuchać. Po chwili namyslu postanawia ci pomóc. Możesz uznać swój dług za spłacony"
                putStrLn "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że masz przyszłość"
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                putStrLn "Dziekan wydaje się słuchać ciebie z uwagą, jednak w połowie twojej historii o tragedii kiedy twój trzeci pies zjadł również twój laptop, podczas pogrzebu twojej babci, zaczyna się z ciebie śmiać. Nie rozwiążesz tego rozmową"
                let newGameState = makeUncooperative gameState npc
                return newGameState

        "4" -> do

            let strzalCheck = any (\item -> itemName item == "Zloty strzal") (inventory gameState)
            if strzalCheck then do
                putStrLn "Dziekan stoi chwilę oniemiały. był pewny że to tylko legenda. Jednak wie dobrze że jest to coś co musi uhonorować"
                putStrLn "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że masz przyszłość"
                let newGameState = gameState { running = False } 
                return newGameState
                
            else do
                putStrLn "Dziekan śmieje się z ciebie i pyta czy to też pies ci zjadł"
                return (makeUncooperative gameState npc)


        _ -> do  
            putStrLn "Dziekan nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi"
            return gameState 


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
        putStrLn "Maszyna wydaje się buczyć 'Ach, wreszcie godny oponent'"
        putStrLn "Po czym wybucha drastycznie ciebie raniąc. Sfrustrowany dziekan podziwia twoje poświęcenie oglądac jak twoje nieprzytomne ciało jest wywożone z wydziału ambulansem bez uiszczenia opłaty"
        putStrLn "Koniec gry. Nie będziesz się musiał martwić warunkami przez bardzo długi czas"
        let newGameState = gameState { running = False } 
        return newGameState
        
    _ -> do 
        -- +/- 2 do ataku bazowego minus defense przeciwnika 
        randOffset <- randomRIO ((0) :: Int, (4) :: Int) 
        let
            damageNPC = (herosAttack (stats gameState)) + (randOffset - 2) -  (defense npc)
            halfwayGameState = updateNPC gameState damageNPC
        putStrLn ("zadajesz " ++ show damageNPC ++ " obrazenia")

        randOffset <- randomRIO ((0) :: Int, (4) :: Int) 
        let
            damageHero = (attack npc) + (randOffset - 2) -  (herosDefense (stats gameState))
            finalGameState = updateHero halfwayGameState damageHero
        putStrLn ( npcName npc ++ " zadaje ci " ++ show damageHero ++ " obrazen")


        if ((herosEnergy (stats gameState)) - damageHero < 0) then do
            putStrLn "Koniec gry. Zostałeś pokonany"
            let newGameState = gameState { running = False } 
            return newGameState

        else if ((energy npc) - damageNPC < 0) then do
            putStrLn ("I jak stał. " ++ npcName npc ++ " teraz już nie stoi")
            return finalGameState 

        else do 
            putStrLn ("przeciwnikowi zostało " ++ show ((energy npc) - damageNPC) ++ " energii")
            putStrLn ("zostało ci " ++ show (herosEnergy (stats finalGameState)) ++ " energii\n")
            attackNPC finalGameState

