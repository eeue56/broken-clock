import Html exposing (div, button, text, h1)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style)
import Color exposing (lightBlue, lightYellow, black, darkRed, lightOrange, green)

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
  | hour < 6 -> lightBlue
  | hour < 13 -> lightYellow
  | hour > 21 -> black
  | hour > 16 ->  darkRed
  | hour > 12 -> lightOrange
  | otherwise -> green

quarterly = fromTime >> hour >> quarterlyInWords

quaterlyView model = h1 [style [("backgroundColor", model.time |> quarterlyInColor |> toString)]] [ 
  model.time 
    |> quarterly 
    |> text 
 ]

view model =
  div []
    [quaterlyView model]

update newTime =
  { model | time <- newTime }
  |> view

main =
  Signal.map update clockSignal
