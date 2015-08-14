module BrokenClock where
import Html exposing (div, text, h1, option, select, fromElement)
import Html.Events exposing (targetChecked, targetValue, on, onClick)
import Time exposing (every, second)
import Date exposing (fromTime, hour)
import Html.Attributes exposing (style, class, value, selected)

import Graphics.Element exposing (show)

import Signal exposing ((<~))

import Quarterly exposing (..)
import Views exposing (..)
import Clock exposing (..)
import Updates exposing (..)

actions : Signal.Mailbox Update
actions = Signal.mailbox Updates.Nothing

model : Clock
model = { time = Time.second, 
  clockType = Quarterly }

clockSignal = Signal.map (TimeUpdate) <| every second

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
  Updates.Nothing -> clock


startModel : Signal Clock
startModel = Signal.foldp 
  update 
  model 
  <|
    Signal.merge 
      actions.signal
      clockSignal

main = Signal.map (view actions.address) startModel