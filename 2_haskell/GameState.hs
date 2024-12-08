module GameState where 

import GameMap
import Locations
import ItemsNPCs


data Stats = Stats
    { herosEnergy :: Int
    , herosAttack :: Int
    , herosDefense :: Int
    , herosCharisma :: Int
    }
    deriving (Show)


data GameState = GameState 
    { currentLocation :: Location
    , inventory :: [Item]
    , itemsMap :: ItemsLocations
    , npcsMap :: NPCLocations
    , stats :: Stats
    , running :: Bool
    }