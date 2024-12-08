module GameMap where

import System.Random (randomRIO)
import Data.List (find, intercalate)

import Locations
import ItemsNPCs
import IOFunctions

reverseDirection :: String -> String
reverseDirection "n" = "s"
reverseDirection "s" = "n"
reverseDirection "w" = "e"
reverseDirection "e" = "w"
reverseDirection "up" = "down"
reverseDirection "down" = "up"

type Path = (Location, String, Location)

paths :: [Path]
paths =
  [ --first floor
    (location1_1_a, "e", location1_1_b),
    (location1_1_b, "e", location1_1_c),
    (location1_1_c, "s", location1_2_c),
    (location1_2_c, "w", location1_2_b),

    (location1_1_a, "s", location1_2_a),
    (location1_2_a, "s", location1_3_a),
    (location1_3_a, "e", location1_3_b),
    (location1_3_b, "s", location1_4_b),

    (location1_4_b, "e", location1_4_c),
    (location1_4_c, "n", location1_3_c),
    (location1_3_c, "e", location1_3_d),

    (location1_4_b, "w", location1_4_a),
    (location1_4_a, "s", location1_5_a),
    (location1_5_a, "e", location1_5_b),
    (location1_5_b, "e", location1_5_c),
    (location1_5_c, "e", location1_5_d),
    (location1_5_d, "n", location1_4_d),
    (location1_4_d, "e", location1_4_e),

    (location1_4_e, "s", location1_5_e),
    (location1_5_e, "e", location1_5_f),

    (location1_4_e, "n", location1_3_e),

    (location1_3_e, "e", location1_3_f),
    (location1_3_f, "s", location1_4_f),
    (location1_4_f, "e", location1_4_g),

    (location1_4_g, "up", location2_1_a),

    (location1_3_e, "n", location1_2_e),
    (location1_2_e, "w", location1_2_d),
    (location1_2_d, "n", location1_1_d),
    (location1_1_d, "e", location1_1_e),
    (location1_1_e, "e", location1_1_f),

    (location1_1_f, "s", location1_2_f),
    (location1_2_f, "e", location1_2_g),
    (location1_2_g, "s", location1_3_g),
    (location1_3_g, "e", location1_3_h),
    (location1_3_h, "n", location1_2_h),

    (location1_1_f, "e", location1_1_g),

    (location1_1_g, "s", location1_2_g),

    (location1_1_g, "e", location1_1_h),
    (location1_1_h, "e", location1_1_i),
    (location1_1_i, "e", location1_1_j),
    (location1_1_j, "s", location1_2_j),
    (location1_2_j, "w", location1_2_i),
    (location1_2_i, "s", location1_3_i),
    (location1_3_i, "s", location1_4_i),

    (location1_4_i, "e", location1_4_j),
    (location1_4_j, "n", location1_3_j),

    (location1_4_i, "w", location1_4_h),
    (location1_4_h, "s", location1_5_h),

    (location1_5_h, "w", location1_5_g),

    (location1_5_h, "e", location1_5_i),
    (location1_5_i, "e", location1_5_j),

    --second floor
    (location2_1_a, "e", location2_1_b),
    (location2_1_b, "e", location2_1_c),
    (location2_1_c, "s", location2_2_c),

    (location2_2_c, "w", location2_2_b),
    (location2_2_b, "s", location2_3_b),
    (location2_3_b, "s", location2_4_b),

    (location2_4_b, "e", location2_4_c),
    (location2_4_c, "e", location2_4_d),

    (location2_4_b, "s", location2_5_b),
    (location2_5_b, "w", location2_5_a),
    (location2_5_a, "n", location2_4_a),
    (location2_4_a, "n", location2_3_a),
    (location2_3_a, "n", location2_2_a),

    (location2_2_c, "s", location2_3_c),
    (location2_3_c, "e", location2_3_d),

    (location2_3_d, "n", location2_2_d),
    (location2_2_d, "n", location2_1_d),

    (location2_3_d, "e", location2_3_e),

    (location2_3_e, "n", location2_2_e),
    (location2_2_e, "n", location2_1_e),

    (location2_3_e, "s", location2_4_e),
    (location2_4_e, "s", location2_5_e),
    (location2_5_e, "w", location2_5_d),
    (location2_5_d, "w", location2_5_c)
  ]

bidirectionalPaths :: [Path] -> [Path]
bidirectionalPaths ps = ps ++ map reversePath ps
    where
        reversePath (from, direction, to) = (to, reverseDirection direction, from)

describeLocation :: Location -> String
describeLocation (Location floor location) 
    | location == "start" = "Jesteś na " ++ show floor ++ ". piętrze, przed dziekanatem."
    | location == "staircase 1" = "Jesteś na " ++ show floor ++ ". piętrze koło schodów na drugie piętro."
    | location == "staircase 2" = "Jesteś na " ++ show floor ++ ". piętrze koło schodów na pierwsze piętro."
    | location == "end" = "Jesteś na " ++ show floor ++ ". piętrze przy wyjściu."
    | otherwise = "Jesteś na " ++ show floor ++ ". piętrze, korytarz " ++ location

type ItemsLocations = [(Item, Location)]

initialItems :: ItemsLocations
initialItems = [ (poor_notes, location1_3_d), (outdated_regulations, location1_2_b) ]

randomSelect list = do
    idx <- randomRIO (0, length list - 1)
    let (before, x:after) = splitAt idx list
    return (x, before ++ after)

addItemInRandomLocation :: Item -> [Location] -> ItemsLocations -> IO (ItemsLocations, [Location])
addItemInRandomLocation item potentialLocations currentItems = do
    (location, remainingLocations) <- randomSelect potentialLocations
    let newItemLocation = (item, location)
    return (currentItems ++ [newItemLocation], remainingLocations)

type NPCLocations = [(NPC, Location)]

initialNPCs = []

addNPCInRandomLocation :: NPC -> [Location] -> NPCLocations -> IO (NPCLocations, [Location])
addNPCInRandomLocation npc potentialLocations currentNPCs = do
    (location, remainingLocations) <- randomSelect potentialLocations
    let newNPCLocation = (npc, location)
    return (currentNPCs ++ [newNPCLocation], remainingLocations)

chooseRandomLocations :: ItemsLocations -> NPCLocations -> IO (ItemsLocations, NPCLocations)
chooseRandomLocations currentItems currentNPCs = do
    let 
        potentialJackpotLocations = [location2_4_d, location2_2_a, location2_1_d, location2_1_e, location2_5_c]
        potentialNPCLocations = [location1_3_e, location1_2_g, location1_3_j, location1_5_g]
   
    (updatedNPCs1, remainingLocations1) <- addNPCInRandomLocation lecturer potentialNPCLocations currentNPCs
    (updatedNPCs2, remainingLocations2) <- addNPCInRandomLocation vendingMachine remainingLocations1 updatedNPCs1
    (updatedItems, remainingLocations3) <- addItemInRandomLocation jackpot potentialJackpotLocations currentItems
    (updatedNPCs3, remainingLocations4) <- addNPCInRandomLocation dean remainingLocations3 updatedNPCs2
    (updatedNPCs4, remainingLocations5) <- addNPCInRandomLocation dean remainingLocations4 updatedNPCs3
    (updatedNPCs5, remainingLocations6) <- addNPCInRandomLocation dean remainingLocations5 updatedNPCs4
    (updatedNPCs6, remainingLocations7) <- addNPCInRandomLocation dean remainingLocations6 updatedNPCs5
    return (updatedItems, updatedNPCs6)

addJanitor :: NPCLocations -> NPCLocations
addJanitor currentNPCs = currentNPCs ++ [(janitor, location1_5_i)]

encounterNPC :: Location -> NPCLocations -> String
encounterNPC loc npcsMap = 
    case find (\(_, npcLocation) -> npcLocation == loc) npcsMap of
        Just (npc, _) -> 
            "Spotykasz " ++ npcName npc ++ "."
        Nothing -> ""


checkForNPC :: Location -> NPCLocations -> Bool
checkForNPC loc npcsMap = any (\(_, npcLocation) -> npcLocation == loc) npcsMap