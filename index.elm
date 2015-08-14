module BrokenClock where
import Time exposing (every, second)
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