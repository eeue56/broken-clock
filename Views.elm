module Views where

import Html exposing (div, text, h1, option, select, fromElement)
import Html.Events exposing (targetChecked, targetValue, on, onClick)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class, value, selected)

import Quarterly exposing (..)
import Clock exposing (..)
import Updates exposing (..)

justHour = fromTime >> hour 

hourView model = h1 [] [ model.time |> justHour |> Basics.toString |> text ]

typeOption model optionName = option 
      [selected <| optionName == model.clockType] 
      [Basics.toString optionName |> text] 

clockTypeSelectView : Signal.Address Update -> Clock -> Html.Html
clockTypeSelectView address model = select 
    [on "input" targetValue (Signal.message address << TypeUpdate << toClockType),
     class "clock-type-dropdown"] 
    <| List.map (typeOption model) [Hourly, Quarterly]

quarterly = fromTime >> hour >> toQuarterly >> Quarterly.toString

quarterlyView model = let 
    time = model.time |> fromTime |> hour |> toQuarterly
  in  
    div [ 
        style [("backgroundColor", time |> toColor)], 
        class "clock-background" 
    ] [
      div [
        class "clock-time"
      ] 
      [ 
        time 
          |> Quarterly.toString 
          |> text 
     ]
   ]