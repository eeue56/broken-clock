module BrokenClock where
import Html exposing (div, text, h1, option, select, fromElement)
import Html.Events exposing (targetChecked, targetValue, on, onClick)
import Time exposing (every, second)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class, value, selected)

import Graphics.Element exposing (show)

import Signal exposing ((<~))

import Quarterly exposing (..)

type ClockType = Hourly | Quarterly
type Update = TimeUpdate (Time.Time) | TypeUpdate (ClockType) | Nothing

type alias Clock = {
  time : Time.Time,
  clockType: ClockType
}

actions : Signal.Mailbox Update
actions = Signal.mailbox Nothing

toClockType x = if
  | x == "Hourly" -> Hourly
  | otherwise -> Quarterly

model : Clock
model = { time = Time.second, 
  clockType = Quarterly }

clockSignal = Signal.map (TimeUpdate) <| every second


justHour = fromTime >> hour >> Basics.toString
    
hourView model = h1 [] [ model.time |> justHour |> text ]

clockTypeSelectView : Signal.Address Update -> Clock -> Html.Html
clockTypeSelectView address model = let
    typeOption x = option 
      [selected <| x == model.clockType ] 
      [Basics.toString x |> text]
  in
     select [on "input" targetValue (Signal.message address << TypeUpdate << toClockType)] 
       <| List.map typeOption [Hourly, Quarterly]

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

update: Update -> Clock -> Clock
update update clock = case update of
  TimeUpdate newTime -> { clock | time <- newTime }
  TypeUpdate newType -> { clock | clockType <- newType }
  Nothing -> clock


startModel : Signal Clock
startModel = Signal.foldp 
  update 
  model 
  <|
    Signal.merge 
      actions.signal
      clockSignal

main = Signal.map (view actions.address) startModel