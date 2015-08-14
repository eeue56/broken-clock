module Views where

import Html exposing (div, text, option, select, h1, button, fromElement)
import Html.Events exposing (targetValue, on, onClick)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class, value, selected)

import String exposing (toInt)

import Graphics.Element exposing (show)

import Time exposing (second)

import Quarterly exposing (..)
import Clock exposing (..)
import Updates exposing (..)
import Alarms exposing (..)

import GenericViews exposing (..)

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

addAlarmView : Signal.Address Update -> Clock -> Html.Html
addAlarmView address model = button 
  [onClick address (NewAlarm <| Alarm (model.time + second) True False)]
  [text "Add alarm"]

timeToNumber : String -> Int 
timeToNumber time = case toInt time of
  Ok x -> x
  Result.Err _ -> 0

hoursView : Signal.Address Update -> Html.Html
hoursView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Hour << timeToNumber)] 
  1
  24

minutesView : Signal.Address Update -> Html.Html
minutesView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Minute << timeToNumber)] 
  1
  60

secondsView : Signal.Address Update -> Html.Html
secondsView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Second << timeToNumber)] 
  1 
  60


alarmDateView : Signal.Address Update -> Clock -> Html.Html
alarmDateView address clock = 
  div [] [hoursView address, minutesView address, secondsView address]

quarterlyView model = let 
  time = model.time |> justHour |> toQuarterly
  alarms = (List.length <| alarmsGoingOff model.alarms) > 0 
  coloring model = if not alarms then time |> toColor else "orange"
  in  
    div [ 
        style [("backgroundColor", coloring model)], 
        class "clock-background" 
    ] [
      div [
        class "clock-time"
      ] 
      [ 
        time 
          |> Quarterly.toString 
          |> (\x -> if alarms then x ++ "\nDo something! Do something!" else x)
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
    alarmDateView address model, 
    addAlarmView address model,
    mainView model
  ]