module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
import Pathfinder
import RedundantDataManager
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Int


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , action : Action
    , land : List Settlement
    , moveSpeed : Float
    }




{- updateLord : Lord -> Lord
   updateLord s =
       let
           goldIncome =
               foldl (cons ((+) 5)) land 0
       in
       { s | gold = s.gold + goldIncome }
-}



-- https://www.fantasynamegenerators.com/town_names.php
-- around 15 Castle names


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



-- around 30 Villages names


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


type alias Action =
    { actionType : ActionType, actionMotive : ActionMotive }


type ActionMotive
    = AttackLord
    | Siege
    | Defend
    | Flee


type ActionType
    = Wait
    | Travel Point (Maybe Pathfinder.Path)



type alias Settlement =
    { entity : WorldEntity
    , settlementType : SettlementType
    , income : Float
    , isSieged : Bool
    }


type alias WorldEntity =
    { army : List Troop
    , faction : Faction
    , position : Point
    , name : String
    }


setPosition : WorldEntity -> Vector.Point -> WorldEntity
setPosition entity pos =
    { entity | position = pos }


type SettlementType
    = Village
    | Castle


createCapitalFor : WorldEntity -> Settlement
createCapitalFor e =
    { entity = { army = [], faction = e.faction, position = e.position, name = e.name ++ "`s Capital`" }, settlementType = Castle, income = 1.0, isSieged = False }


type alias SettlementInfo =
    { sType : SettlementType
    , position : Vector.Point
    }


getSettlementFor : Lord -> SettlementInfo -> Settlement
getSettlementFor l info =
    { entity = { army = [], faction = l.entity.faction, position = info.position, name = "" }, settlementType = info.sType, income = 1.0, isSieged = False }


combineSettlementName : Settlement -> String
combineSettlementName settlement =
    getSettlementNameByType settlement.settlementType ++ " - " ++ settlement.entity.name


getSettlementNameByType : SettlementType -> String
getSettlementNameByType s =
    case s of
        Village ->
            "Village"

        Castle ->
            "Castle"


settlementImageName : SettlementType -> String
settlementImageName s =
    getSettlementNameByType s ++ ".png"
