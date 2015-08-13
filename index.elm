module BrokenClock where
import Html exposing (div, text, h1)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class)

import Quarterly exposing (..)

type ClockType = Hourly | Quarterly

model = { time = 0, clockType = Quarterly }

clockSignal = every second


justHour = fromTime >> hour >> Basics.toString
    
hourView model = h1 [] [ model.time |> justHour |> text ]



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



view model = case model.clockType of 
  Hourly -> hourView model
  Quarterly -> quarterlyView model

update newTime =
  { model | time <- newTime }
  |> view

main =
  Signal.map update clockSignal
