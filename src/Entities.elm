module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
import Pathfinder
import RedundantDataManager
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Float


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , action : Action
    , land : List Settlement
    , moveSpeed : Float
    }


updateEntitiesArmy : List Troop -> WorldEntity -> WorldEntity
updateEntitiesArmy l e = 
        {e | army = l}


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

-- calc income
calculateRoundIncome: Lord -> Float
calculateRoundIncome lord =
                sumSettlementsIncome lord.land - sumTroopWages (flattenTroops (sumLordTroops lord) Troops.troopTypeList)

sumSettlementsIncome : List Settlement -> Float
sumSettlementsIncome s = 
    foldr (\x v -> x.income + v) 0 s

sumTroopWages : List Troop -> Float
sumTroopWages t =
    foldr (\x v -> toFloat x.amount * troopWage x.troopType + v) 0 t

sumLordTroops : Lord -> List Troop
sumLordTroops lord =
        lord.entity.army ++ foldr (\x y -> x.entity.army ++ y) [] lord.land

-- refactor it
flattenTroops : List Troop -> List TroopType -> List Troop
flattenTroops troops types = 
            case types of
                [] -> 
                    []

                (y :: ys) ->
                    {amount = List.foldr (\t v-> t.amount + v) 0 (List.filter (\ x -> x.troopType == y) troops), troopType = y} :: flattenTroops troops ys


