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


itemFromMachine :: GameState -> IO (GameState, String)
itemFromMachine gameState = do
    idx <- randomRIO (0, length vendingMachineInventory - 1)
    let 
        item = vendingMachineInventory !! idx
        newInventory = item : (inventory gameState)
        newGameState = updateStatsAfterTake gameState item 

    return (newGameState { inventory = newInventory }, "Otrzymujesz: " ++ (itemName item))


talkWithNPC :: GameState -> NPC -> IO GameState
talkWithNPC gameState npc = 
  case npcName npc of
    "Wykładowcę" -> do 
      printLines ["Witasz wykładowcę, który zaczyna mówić o twoich problemach w nauce", "1) starasz się wytłumaczyć", "2) obrażony wyzywasz go", "3) próbujesz go zmanipulować by ci pomógł"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            printLines ["Wykładowca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie"]
            return gameState

        "2" -> do
            printLines ["Wykładowca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: 'Dosć padlo słów, czas podyskutować'"]
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                printLines ["Dzięki odrobinie charyzmy wykadowca zdradza ci sekret. Przed wyjściem powinien właśnie teraz stać dozorca. Jest to rozsądny człowiek może uda ci się z nim dogadać"]
                let 
                    newMap = addJanitor (npcsMap gameState)
                    newGameState = gameState {npcsMap = newMap}
                return newGameState
            else do  
                printLines ["Akurat kiedy myślałeś że wykładowca za chwilę powie coś ważnego, nagle twarz mu pochmurnieje. Zauważył co próbowałeś zrobić i nie jest zadowolony"]
                let newGameState = makeUncooperative gameState npc
                return newGameState

        _ -> do 
            printLines ["Wykładowca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?"]
            return gameState 


    "Automat" -> do 
      printLines ["Automat buczy nieznacznie, czy masz na coś ochotę?", "1) potrzebuję się napić", "2) przydałyby mi się jakieś lepsze notatki", "3) chyba nic nie potrzebuję"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "3" -> do
            printLines ["Chyba rzeczywiście to nie czas na zakupy."]
            return gameState
        _ -> do 
            (newGameState, message) <- itemFromMachine gameState
            printLines ["Wpisujesz kod, jednak automat wydaje się nie przejmować zupełnie twoimi instrukcjami, podając ci coś innego.", message]

            return (makeUncooperative newGameState npc)
  


    "Dozorcę" -> do 
      printLines ["Witasz Dozorcę, jednak ten wydaje się ciebie nie słuchać", "1) tłumaczysz swoją sytuację", "2) obrażony każesz mu otworzyć drzwi w mniej niż uprzejmy sposób", "3) próbujesz ponownie wykorzystać swoją charyzmę"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            printLines ["Dozorca potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie."]
            return gameState

        "2" -> do
            printLines ["Dozorca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padlo słów, czas podyskutować"]
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                printLines ["Odrobina uprzejmości i dozorca od razu się do ciebie otwiera. Słuchasz jego historii jak sam był kiedyś studentem, ale warunki zniszczyły mu życie. Po rozmowie postanawia się cie wypuścić. Udało ci się uciec, ale nadal nie rozwiązałeś swoich problemów", "Koniec gry, przeżyłeś dzisiejszy dzień, ale jest to życie pełne strachu i niepewności. Czy to jest to czego chciałeś?"]
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                printLines ["Jak się okazuje, dozorca nie ma najmniejszej ochoty z tobą rozmawiać, a twoje natręctwo tylko go zirytowało."]
                let newGameState = makeUncooperative gameState npc
                return newGameState

        _ -> do 
            printLines ["Dozorca nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?"]
            return gameState 



    "Dziekana" -> do 
      printLines ["Witasz Dziekana, jednak ten nie wydaje się chętny do słuchania wymówek", "1) tłumaczysz swoją sytuację", "2) obrażony nonszalancją dziekana wybuchasz i mówisz dwa razy więcej niż powinieneś", "3) próbujesz ponownie wykorzystać swój urok osobisty", "4) mówisz dziekanowi że masz ze sobą legendarny złoty strzał"]
      cmd <- readCommand
      let (action, argument) = parseCommand cmd
      case action of
        "1" -> do
            printLines ["Dziekan potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie."]
            return gameState

        "2" -> do
            printLines ["Dziekan wydaje się obrażony, możliwe że powiedziałeś za dużo."]
            return (makeUncooperative gameState npc)
        

        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 1) then do
                printLines ["Dziekan słucha cię uważnie, twoje idealnie dobrane odzienie, naturalna charyzma i umniejętność zmyślania historii w czasie rzeczywistym, sprawiają że nie może ciebie nie wysłuchać. Po chwili namyslu postanawia ci pomóc. Możesz uznać swój dług za spłacony", "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że przyszłość jest w twoich rękach."]
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                printLines ["Dziekan wydaje się słuchać ciebie z uwagą, jednak w połowie twojej historii o tragedii kiedy twój trzeci pies zjadł również twój laptop podczas pogrzebu twojej babci, zaczyna się z ciebie śmiać. Nie rozwiążesz tego rozmową."]
                let newGameState = makeUncooperative gameState npc
                return newGameState

        "4" -> do

            let strzalCheck = any (\item -> itemName item == "Zloty strzal") (inventory gameState)
            if strzalCheck then do
                printLines ["Dziekan stoi przez chwilę oniemiały, był pewny że to tylko legenda. Jednak wie dobrze że jest to coś co musi uhonorować", "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że masz przyszłość"]
                let newGameState = gameState { running = False } 
                return newGameState
                
            else do
                printLines ["Dziekan stoi przez chwilę oniemiały, był pewny śmieje się z ciebie i pyta czy to też pies ci zjadł"]
                return (makeUncooperative gameState npc)


        _ -> do  
            printLines ["Dziekan nie wydaje się zachwycony twoją odpowiedzią, może spróbuj uważać na to co się do ciebie mówi?"]
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

