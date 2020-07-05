module Building exposing (..)


type alias Building = 
    {
        name: String
        , level: Int
        , buildingType: BuildingType
    }

type BuildingType
    = Marketplace
    | Barracks
    | Fortress

startBuildings : List Building
startBuildings = [
    { name = "Marketplace", level = 0, buildingType = Marketplace}
    { name = "Barracks", level = 0, buildingType = Barracks}
    { name = "Fortress", level = 0, buildingType = Fortress}
]