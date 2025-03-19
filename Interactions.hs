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
    printLines ["Witasz wykładowcę, który zaczyna mówić o twoich problemach w nauce.",
                "Pragnie cię odpytać czy jedynie porozmawiać?",
                "1  -- Starasz się wytłumaczyć",
                "2  -- Obrażony wyzywasz go",
                "3  -- Próbujesz go zmanipulować by ci pomógł", ""]
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
                printLines ["Starasz się zrobić dobre wrażenie i odpowiadasz mu 'Dzień dobry panie magistrze doktorze doktorancki od spraw bardzo ważnych'. Nie przemyślałeś jednak że przywitanie może nie być najmądrzejszym pomysłem kiedy wykładowca oczekuje od ciebie wyjaśnień. Twarz mu nagle pochmurnieje, a z jego ust zaczynają się sypać pytania dotyczące twojej wiedzy.", ""]
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
                    "1  -- Potrzebuję kopa, może jakiś energetyk?", 
                    "2  -- Przydałyby mi się jakieś lepsze notatki.", 
                    "3  -- Chyba nic nie potrzebuję...",
                    "4  -- Uderz w automat, może się uda wyciągnąć kilka rzeczy", ""]
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
                printLines ["Wklepujesz swój wybór w automat, jednak on ani drgnie. Czy na pewno się nie pomyliłeś?", ""]
                interactWithVendingMachine gameState
  where
    itemAction :: GameState -> IO GameState
    itemAction gs = do
        (newGameState, message) <- itemFromMachine gs
        printLines ["Wpisujesz kod, jednak automat wydaje się nie przejmować zupełnie twoimi instrukcjami podając ci coś innego. Wraz z wyrzuceniem przedmiotu, światełko w automacie zaczyna intensywnie migać, aż w końcu gaśnie.", "", message, ""]
        let updatedGameState = newGameState { vendingMachineUsed = True } 
        return updatedGameState
        

      
interactWithJanitor :: GameState -> IO GameState
interactWithJanitor gameState = do
    printLines ["Zauważasz starszego mężczyznę stojącego przed wejściem. Czy zawsze tutaj stał? Jego posiwiałe włosy i zmarszczki opowiadają historię o zmarnowanej młodości i życiu pełnego stresu i żalów. Jednak nie masz czasu na sympatię. Prosisz go żeby cię wypuścił, jednak on zdaje się ciebie nie słuchać.",
                "1  -- Tłumaczysz swoją sytuację", 
                "2  -- Obrażony, wybuchasz oraz każesz mu otworzyć drzwi w mniej niż uprzejmy sposób",
                "3  -- Próbujesz ponownie wykorzystać swoją charyzmę", ""]
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "1" -> do
            printLines ["Dozorca wciąż siedzi bez ruchu, ignoruje twoje tłumaczenia czy nawet ich nie słyszy?", ""]
            interactWithJanitor gameState

        "2" -> do
            printLines ["Dozorca wydaje się obrażony, możliwe że powiedziałeś za dużo. Słyszysz jak mówi: dosć padło słów, czas podyskutować", ""]
            attackNPC gameState
        
        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 0) then do
                printLines ["Odrobina uprzejmości i dozorca jakby nagle ożył. Jednak po pewnym pytaniu zamiera ci serce.",
                            "- 'Hej, czy to aby nie ty nie opłaciłeś warunku i chcesz teraz wyjść na pewniaka, bez konfrontacji z dziekanem?' - pyta",
                            "Milczysz, ale boisz się najgorszego",
                            "'Wiesz', zaczyna powoli, 'właściwie to przypominasz mi mnie samego z czasów studiów. Też walczyłem z systemem, dyskutowałem, kombinowałem, jak ominąć regulamin. Niestety nie udało się i zobacz, jak skończyłem. Nie chciałbym, żeby kogoś jeszcze to spotkało.'",
                            "Dozorca ustąpił, a ty wychodzisz z uczelni.",
                            "",
                            "Udało Ci się ominąć stróża i wydostajesz się z budynku. Czujesz rosnącą ulgę z każdym krokiem oddalającym cię od biura dziekana. Na moment wydaje się, że wszystko jest w porządku - jakbyś naprawdę przechytrzył system. Twoje długi jednak nie zniknęły i nie będzie można przed nimi uciekać w nieskończoność. Odsetki od twojego długu wciąż rosną, a kolejne listy z uczelni zaczną spływać szybciej, niż zdążysz znaleźć kryjówkę, zmienić tożsamość lub znaleźć prawdziwe rozwiązanie swojeego problemu.",
                            "Koniec gry.", "", ""]
                let newGameState = gameState { running = False } 
                return newGameState

            else do  
                printLines ["Jak się okazuje, dozorca nie ma najmniejszej ochoty z tobą rozmawiać, a twoje natręctwo tylko go zirytowało.", ""]
                attackNPC gameState
        _ -> do 
            printLines ["Bełkoczesz coś pod nosem, oczywiście dozorcy to nie rusza. Musisz podjąć jakąś decyzję.", ""]
            interactWithJanitor gameState



interactWithDean :: GameState -> IO GameState
interactWithDean gameState = do 
    printLines ["Zauważasz niewielki błysk na końcu korytarza. Z zaciekawieniem postanawisz podejść bliżej, jednak szybko żałujesz tej decyzji. Z cienia wyłania się dziekan szczerząc się do ciebie. Witasz go, jednak ten nie wydaje się chętny do słuchania wymówek.",
                "'Twoje żałosne próby zdania semestru kosztowały mnie już wystarczająco dużo czasu. Twój czas dobiegł końca' - mówi z sadystyczną satysfakcją",
                "1  -- Tłumaczysz swoją sytuację",
                "2  -- Obrażony nonszalancją dziekana wybuchasz i mówisz dwa razy więcej niż powinieneś",
                "3  -- Próbujesz wykorzystać swój urok osobisty",
                "4  -- Mówisz dziekanowi że masz ze sobą legendarny złoty strzał", ""]
    cmd <- readCommand
    let (action, argument) = parseCommand cmd
    case action of
        "1" -> do
            printLines ["Dziekan potakuje ruchem głowy wyćwiczonym do perfekcji na setkach studentów przed tobą, mającym reprezentować jego współczucie.", ""]
            interactWithDean gameState

        "2" -> do
            printLines ["Dziekan wydaje się obrażony, możliwe że powiedziałeś za dużo... o wiele za dużo", ""]
            attackNPC gameState
        
        "3" -> do
            idx <- randomRIO (0 :: Int, 1 :: Int)
            if (idx == 1 && herosCharisma (stats gameState) > 1) then do
                printLines ["Dziekan słucha cię uważnie, twoje idealnie dobrane odzienie, naturalna charyzma i umniejętność zmyślania historii w czasie rzeczywistym, sprawiają że nie może ciebie nie wysłuchać. Gdy skończyłeś, przysiągłbyś, że widziałeś łezkę zbierającą się w jego oku.", "Po chwili namysłu Dziekan postanawia ci pomóc. Możesz uznać swój dług za spłacony", "Koniec gry, przeżyłeś dzisiejszy dzień i po raz pierwszy od dawna czujesz że przyszłość jest w twoich rękach.", "", ""]
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
                printLines ["Dziekan stoi przez chwilę oniemiały, jednak szybko odzyskuje równowagę, widząc, że to tylko blef. Zaczyna się śmiać z ciebie i pyta, czy to też pies ci zjadł", ""]
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
        printLines ["Maszyna wydaje się buczyć 'Ach, wreszcie godny oponent...'", "Po czym wybucha drastycznie ciebie raniąc", "Sfrustrowany dziekan podziwia twoje poświęcenie oglądając jak twoje nieprzytomne ciało jest wywożone z wydziału ambulansem bez uiszczenia opłaty", "Nigdy by mu nie przyszło do głowy że otrzymasz pomoc od jego odwiecznego wroga", "Koniec gry. Nie musisz się już martwić warunkami, właściwie nie martwisz się już o nic..."]
        let newGameState = gameState { running = False } 
        return newGameState
        
    _ -> do 
        -- +/- 2 do ataku bazowego minus defense przeciwnika 
        randOffset <- randomRIO ((0) :: Int, (2) :: Int) 
        let
            damageNPC = max 0 ((herosAttack (stats gameState)) + randOffset - (defense npc))
            halfwayGameState = updateNPC gameState damageNPC
        printLines ["twój argument zabija " ++ show damageNPC ++ " szarych komórek " ++ npcName npc]

        randOffset <- randomRIO ((0) :: Int, (2) :: Int) 
        let
            damageHero = max 0 ((attack npc) + randOffset -  (herosDefense (stats gameState)))
            finalGameState = updateHero halfwayGameState damageHero
        printLines ["Próbujesz się bronić regulaminem, ale tłumaczący " ++ npcName npc ++ " ledwo daje ci dojść do słowa. tracisz " ++ show damageHero ++ " energii próbując udawać że rozumiesz o czym mówi"]


        if ((herosEnergy (stats gameState)) - damageHero < 0) then do
                printLines ["To koniec, ledwo stoisz na nogach. Nie masz już siły z nim rozmawiać.", "Przegrałeś."]
                let endGameState = finalGameState { running = False } 
                return endGameState

        else if ((energy npc) - damageNPC <= 0) then do
            if npcName npc == npcName janitor then do 
                printLines ["Dozorca został pokonany, nic nie stoi ci na drodze do wyjścia. Ale nadal czujesz ogromny ciężar na sercu, wiesz że to jeszcze nie koniec...", "Koniec gry."]
                let endGameState = finalGameState { running = False } 
                return endGameState

            else do
                printLines ["I jak stał. " ++ npcName npc ++ " teraz już nie stoi", "Namieszałeś mu w głowie tak bardzo, że nie wie już nawet jak się nazywa"]
                return finalGameState 

        else do 
            printLines ["Przeciwnikowi zostało " ++ show ((energy npc) - damageNPC) ++ " szarych komórek do zniszczenia", "Zostało ci " ++ show (herosEnergy (stats finalGameState)) ++ " energii.", ""]
            attackNPC finalGameState

