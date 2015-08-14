module BrokenClock where
import Time exposing (every, second)
import Signal exposing ((<~))

import Quarterly exposing (..)
import Views exposing (..)
import Clock exposing (..)
import Updates exposing (..)
import Alarms exposing (Alarm)

actions : Signal.Mailbox Update
actions = Signal.mailbox Updates.Nothing

model : Clock
model = { time = Time.second, 
  clockType = Quarterly,
  alarms = [] }


clockSignal = Signal.map (TimeUpdate) <| every second

update: Update -> Clock -> Clock
update update clock = case update of
  TimeUpdate newTime -> { clock | time <- newTime }
  TypeUpdate newType -> { clock | clockType <- newType }
  NewAlarm alarm -> { clock | alarms <- (alarm :: clock.alarms) }
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