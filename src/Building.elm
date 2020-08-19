module Building exposing (..)

import OperatorExt


type alias Building =
    { name : String
    , level : Int
    , buildingType : BuildingType
    }


type BuildingType
    = Quarters
    | Barracks
    | Fortress


startBuildings : List Building
startBuildings =
    [ { name = "Quarters", level = 0, buildingType = Quarters }
    , { name = "Barracks", level = 0, buildingType = Barracks }
    , { name = "Fortress", level = 0, buildingType = Fortress }
    ]


upgradeBuildingCost : Building -> Float
upgradeBuildingCost b =
    upgradeBuildingInfoCost b.buildingType (b.level + 1)


upgradeBuildingInfoCost : BuildingType -> Int -> Float
upgradeBuildingInfoCost t l =
    upgradeCostBase t + (upgradeCostBase t / 2) * Basics.toFloat l


buildingToBonus : BuildingType -> Float
buildingToBonus b =
    case b of
        Quarters ->
            2

        Barracks ->
            5

        Fortress ->
            10


buildingToBonusInfo : BuildingType -> Int -> String
buildingToBonusInfo b i =
    case b of
        Quarters ->
            "+" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ " recruit space in all settlements"

        Barracks ->
            "+" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ "% attacker-bonus in offensive battles"

        Fortress ->
            "+" ++ String.fromFloat (Basics.toFloat i * buildingToBonus b) ++ "% defender-bonus in the capital "


upgradeCostBase : BuildingType -> Float
upgradeCostBase b =
    case b of
        Quarters ->
            550

        Barracks ->
            350

        Fortress ->
            500


upgradeBuildingType : List Building -> BuildingType -> List Building
upgradeBuildingType b bt =
    List.map (\x -> { x | level = OperatorExt.ternary (x.buildingType == bt) (x.level + 1) x.level }) b


getBuilding : BuildingType -> List Building -> Maybe Building
getBuilding t bs =
    List.head <| List.filter (\b -> b.buildingType == t) bs


resolveBonusFromBuildingInfo : BuildingType -> Int -> Float
resolveBonusFromBuildingInfo t l =
    buildingToBonus t * toFloat l


resolveBonusFromBuilding : Building -> Float
resolveBonusFromBuilding b =
    resolveBonusFromBuildingInfo b.buildingType b.level


resolveBonusFromBuildings : List Building -> BuildingType -> Float
resolveBonusFromBuildings bs t =
    case getBuilding t bs of
        Nothing ->
            0

        Just b ->
            resolveBonusFromBuilding b
