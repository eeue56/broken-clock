module BrokenClock where
import Html exposing (div, text, h1)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class)

model = { time = 0 }

clockSignal = every second
        
         
justHour = fromTime >> hour >> toString
    
hourView model = h1 [] [ model.time |> justHour |> text ]


quarterlyInWords hour = if
  | hour < 6 -> "Before sunrise"
  | hour < 13 -> "After sunrise"
  | hour > 21 -> "Night"
  | hour > 16 ->  "Evening"
  | hour > 12 -> "After midday"
  | otherwise -> "Err.."

quarterlyInColor hour = if
  | hour < 6 -> "lightBlue"
  | hour < 13 -> "lightYellow"
  | hour > 21 -> "black"
  | hour > 16 ->  "darkred"
  | hour > 12 -> "lightOrange"
  | otherwise -> "green"

quarterly = fromTime >> hour >> quarterlyInWords

quarterlyView model = let 
  time = model.time |> fromTime |> hour
  in  
    div [ 
        style [("backgroundColor", time |> quarterlyInColor)], 
        class "clock-background" 
    ] [
      div [
        class "clock-time"
      ] 
      [ 
        time 
          |> quarterlyInWords 
          |> text 
     ]
   ]

view = quarterlyView

update newTime =
  { model | time <- newTime }
  |> view

main =
  Signal.map update clockSignal
