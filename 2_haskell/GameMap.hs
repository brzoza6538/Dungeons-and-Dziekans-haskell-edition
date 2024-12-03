module GameMap where

data Location = Location { floor :: Int, location :: String }
  deriving (Show, Eq)

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
  [ --fisrt floor
    (Location 1 "start", "e", Location 1 "1 b"),
    (Location 1 "1 b", "e", Location 1 "1 c"),
    (Location 1 "1 c", "s", Location 1 "2 c"),
    (Location 1 "2 c", "w", Location 1 "2 b"),

    (Location 1 "start", "s", Location 1 "2 a"),
    (Location 1 "2 a", "s", Location 1 "3 a"),
    (Location 1 "3 a", "e", Location 1 "3 b"),
    (Location 1 "3 b", "s", Location 1 "4 b"),

    (Location 1 "4 b", "e", Location 1 "4 c"),
    (Location 1 "4 c", "n", Location 1 "3 c"),
    (Location 1 "3 c", "e", Location 1 "3 d"),

    (Location 1 "4 b", "w", Location 1 "4 a"),
    (Location 1 "4 a", "s", Location 1 "5 a"),
    (Location 1 "5 a", "e", Location 1 "5 b"),
    (Location 1 "5 b", "e", Location 1 "5 c"),
    (Location 1 "5 c", "e", Location 1 "5 d"),
    (Location 1 "5 d", "n", Location 1 "4 d"),
    (Location 1 "4 d", "e", Location 1 "4 e"),

    (Location 1 "4 e", "s", Location 1 "5 e"),
    (Location 1 "5 e", "e", Location 1 "5 f"),

    (Location 1 "4 e", "n", Location 1 "3 e"),

    (Location 1 "3 e", "e", Location 1 "3 f"),
    (Location 1 "3 f", "s", Location 1 "4 f"),
    (Location 1 "4 f", "e", Location 1 "staircase 1"),

    (Location 1 "staircase 1", "up", Location 2 "staircase 2"),

    (Location 1 "3 e", "n", Location 1 "2 e"),
    (Location 1 "2 e", "w", Location 1 "2 d"),
    (Location 1 "2 d", "n", Location 1 "1 d"),
    (Location 1 "1 d", "e", Location 1 "1 e"),
    (Location 1 "1 e", "e", Location 1 "1 f"),

    (Location 1 "1 f", "s", Location 1 "2 f"),
    (Location 1 "2 f", "e", Location 1 "2 g"),
    (Location 1 "2 g", "s", Location 1 "3 g"),
    (Location 1 "3 g", "e", Location 1 "3 h"),
    (Location 1 "3 h", "n", Location 1 "2 h"),

    (Location 1 "1 f", "e", Location 1 "1 g"),

    (Location 1 "1 g", "s", Location 1 "2 g"),

    (Location 1 "1 g", "e", Location 1 "1 h"),
    (Location 1 "1 h", "e", Location 1 "1 i"),
    (Location 1 "1 i", "e", Location 1 "1 j"),
    (Location 1 "1 j", "s", Location 1 "2 j"),
    (Location 1 "2 j", "w", Location 1 "2 i"),
    (Location 1 "2 i", "s", Location 1 "3 i"),
    (Location 1 "3 i", "s", Location 1 "4 i"),

    (Location 1 "4 i", "e", Location 1 "4 j"),
    (Location 1 "4 j", "n", Location 1 "3 j"),

    (Location 1 "4 i", "w", Location 1 "4 h"),
    (Location 1 "4 h", "s", Location 1 "5 h"),

    (Location 1 "5 h", "w", Location 1 "5 g"),

    (Location 1 "5 h", "e", Location 1 "5 i"),
    (Location 1 "5 i", "e", Location 1 "end"),

    --second floor
    (Location 2 "staircase 2", "e", Location 2 "1 b"),
    (Location 2 "1 b", "e", Location 2 "1 c"),
    (Location 2 "1 c", "s", Location 2 "2 c"),

    (Location 2 "2 c", "w", Location 2 "2 b"),
    (Location 2 "2 b", "s", Location 2 "3 b"),
    (Location 2 "3 b", "s", Location 2 "4 b"),

    (Location 2 "4 b", "e", Location 2 "4 c"),
    (Location 2 "4 c", "e", Location 2 "4 d"),

    (Location 2 "4 b", "s", Location 2 "5 b"),
    (Location 2 "5 b", "w", Location 2 "5 a"),
    (Location 2 "5 a", "n", Location 2 "4 a"),
    (Location 2 "4 a", "n", Location 2 "3 a"),
    (Location 2 "3 a", "n", Location 2 "2 a"),

    (Location 2 "2 c", "s", Location 2 "3 c"),
    (Location 2 "3 c", "e", Location 2 "3 d"),

    (Location 2 "3 d", "n", Location 2 "2 d"),
    (Location 2 "2 d", "n", Location 2 "1 d"),

    (Location 2 "3 d", "e", Location 2 "3 e"),

    (Location 2 "3 e", "n", Location 2 "2 e"),
    (Location 2 "2 e", "n", Location 2 "1 e"),

    (Location 2 "3 e", "s", Location 2 "4 e"),
    (Location 2 "4 e", "s", Location 2 "5 e"),
    (Location 2 "5 e", "w", Location 2 "5 d"),
    (Location 2 "5 d", "w", Location 2 "5 c")
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

--TODO: add items and npcs