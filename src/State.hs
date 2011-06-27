{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}
module State (App) where

import Data.Acid            (AcidState(..))
import Happstack.Server     (ServerPartT)
import Control.Monad.Reader (ReaderT)
import State.Foldr          (Foldr)

-- |App - Allows access to the AcidState via ReaderT
type App = ServerPartT (ReaderT (AcidState Foldr) IO)

