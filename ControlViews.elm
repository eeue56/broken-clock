module ControlViews where 

import Html exposing (div, text, option, select, h1, button, fromElement)
import Html.Events exposing (targetValue, on, onClick)
import Html.Attributes exposing (style, class, value, selected)

import String exposing (toInt)
import Random exposing (initialSeed)

import Time exposing (second, minute, hour)

import Updates exposing (..)
import Clock exposing (..)
import Alarms exposing (..)

import GenericViews exposing (..)


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
  [onClick address (NewAlarm <| Alarm (newAlarmTime model) True False)]
  [text "Add alarm"]

addRandomAlarmView : Signal.Address Update -> Clock -> Html.Html
addRandomAlarmView address model =
  button 
    [onClick address (NewAlarm <| Alarm (addRoughTime model.time <| newRandomRoughTime <| initialSeed <| round model.time) True False)]
    [text "Add random future alarm"]

timeToNumber : String -> Int 
timeToNumber time = case toInt time of
  Ok x -> x
  Result.Err _ -> 0

hoursView : Signal.Address Update -> Html.Html
hoursView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Hour << timeToNumber)] 
  0
  23

minutesView : Signal.Address Update -> Html.Html
minutesView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Minute << timeToNumber)] 
  0
  59

secondsView : Signal.Address Update -> Html.Html
secondsView address = numberSelectView 
  [on "input" targetValue (Signal.message address << AlarmTime << Second << timeToNumber)] 
  0 
  59


alarmDateView : Signal.Address Update -> Clock -> Html.Html
alarmDateView address clock = 
  div [] [ text "time til the alarm goes off", hoursView address, minutesView address, secondsView address ]