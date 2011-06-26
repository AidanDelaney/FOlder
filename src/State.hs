{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}
module State where

import Happstack.Data  (Default, Version(..), deriveSerialize, defaultValue, deriveAll)
import Happstack.State ((:+:), Component(..), Dependencies, End, mkMethods)
import Happstack.Data  (defaultValue)
import Data.SafeCopy
import State.Foldr     (Foldr)

-- |top-level application state
-- in this case, the top-level state itself does not contain any state
-- TODO: remove this legacy
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      data AppState = AppState
   |])

$(deriveSerialize ''AppState)
instance Happstack.Data.Version AppState

-- |top-level application component
-- we depend on the Foldr component
instance Component AppState where
  type Dependencies AppState = End
  initialValue = defaultValue
  
-- create types for event serialization
$(mkMethods ''AppState [])
