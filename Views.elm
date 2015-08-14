module Views where

import Html exposing (div, text, option, select, h1, fromElement)
import Html.Events exposing (targetValue, on, onClick)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class, value, selected)

import Graphics.Element exposing (show)

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

quarterlyView model = let 
  time = model.time |> justHour |> toQuarterly
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

mainView model = case model.clockType of 
  Hourly -> hourView model
  Quarterly -> quarterlyView model

view : Signal.Address (Update) -> Clock -> Html.Html
view address model = div [] 
  [
    fromElement <| show model,
    clockTypeSelectView address model, 
    mainView model
  ]