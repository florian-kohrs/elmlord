module Building exposing (..)

import OperatorExt

-- TODO Maybe set level (int) to float

type alias Building =
    { name : String
    , level : Int
    , buildingType : BuildingType
    }


type BuildingType
    = Marketplace
    | Barracks
    | Fortress


startBuildings : List Building
startBuildings =
    [ { name = "Marketplace", level = 0, buildingType = Marketplace }
    , { name = "Barracks", level = 0, buildingType = Barracks }
    , { name = "Fortress", level = 0, buildingType = Fortress }
    ]

buildingToBonus : BuildingType -> Float
buildingToBonus b =
    case b of
        Marketplace ->
            1.5

        Barracks ->
            1

        Fortress ->
            10

buildingToBonusInfo : BuildingType -> Int -> String
buildingToBonusInfo b i =
    case b of
        Marketplace ->
            "+" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ " Ducats per turn"

        Barracks ->
            "+" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ " Recruits per turn"

        Fortress ->
            "-" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ "% Troopcost"

upgradeCostBase : BuildingType -> Float 
upgradeCostBase b = 
    case b of
        Marketplace ->
            550

        Barracks ->
            350

        Fortress ->
            500

upgradeBuildingType : List Building -> BuildingType -> List Building
upgradeBuildingType b bt = 
    List.map (\x -> { x | level = OperatorExt.ternary (x.buildingType == bt) (x.level + 1) x.level}) b

        
resolveBonusFromBuildings : List Building -> BuildingType -> Float
resolveBonusFromBuildings l b = 
    let
        building = List.head (List.filter (\x -> x.buildingType == b) l)
    in
        case building of
            Nothing ->
                0
            
            Just v ->
                buildingToBonus b * Basics.toFloat v.level