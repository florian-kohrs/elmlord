module Entities.Model exposing (..)

import Building
import Dict
import Faction
import List
import PathAgent.Model
import Troops
import Vector


type alias Gold =
    Float


type alias WorldEntity =
    { army : Troops.Army
    , faction : Faction.Faction
    , position : Vector.Point
    , name : String
    }


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , land : List Settlement
    , agent : PathAgent.Model.Agent
    }


type alias Settlement =
    { entity : WorldEntity
    , settlementType : SettlementType
    , recruitLimits : Troops.Army
    , income : Float
    , isSieged : Bool
    , buildings : List Building.Building
    }


type alias SettlementInfo =
    { sType : SettlementType
    , position : Vector.Point
    , name : String
    , faction : Faction.Faction
    }


type SettlementType
    = Village
    | Castle


aiNames : List String
aiNames =
    [ "Player"
    , "Jan von Haskell"
    , "Herzog K. Willi"
    , "Sir Quicknuss"
    ]



-- around 15 names for castles and 30 for villages
-- https://www.fantasynamegenerators.com/town_names.php was used as a source


castleNames : List String
castleNames =
    [ "Stathford"
    , "Wingston"
    , "Boroughton"
    , "Peterbrugh"
    , "Wimborne"
    , "Westwend"
    , "Kingcardine"
    , "Helmfirth"
    , "Accrington"
    , "Mournstead"
    , "Alcombey"
    , "Aeberuthey"
    , "Bradford"
    , "Bamborourgh"
    , "Everton"
    ]


villageNames : List String
villageNames =
    [ "Haran"
    , "Hillfar"
    , "Waekefield"
    , "Sudbury"
    , "Murkwell"
    , "Caerfyrddin"
    , "Llanybydder"
    , "Galssop"
    , "Farnworth"
    , "Porthaethwy"
    , "Favorsham"
    , "Kilead"
    , "Kald"
    , "Holsworthy"
    , "Wolfwater"
    , "Southwold"
    , "Marnmouth"
    , "Kilmarnock"
    , "Far Water"
    , "Aylesbury"
    , "Dornwich"
    , "Haran"
    , "Murkwell"
    , "Drumnacanvy"
    , "Waeldestone"
    , "Bracklewhyte"
    , "Peatsland"
    , "Ballachulish"
    , "Arbington"
    , "Torrine"
    ]
