{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
module State.Foldr
  (    Foldr(..)
     , DocId(..)
     , getDocument
     , addDocument
  ) where

import Data.Acid
import Control.Applicative               ( (<$>) )
import Control.Monad.Reader              (ask)
import Control.Monad.State               (modify, get, put)
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Data.IxSet              (IxSet(..), (@=))
--import Happstack.State                   (Dependencies, End, mkMethods)
import Types.Foldr

-- | get a 'Document' from a 'Foldr'
getDocument :: DocId -> Query Foldr (IxSet Document)
getDocument id
    = do Foldr documents <- ask
         return $ documents @= (id)
  
-- | add a 'Document' to a 'Foldr'
addDocument :: Document -> Update Foldr ()
addDocument doc = do Foldr documents <- get
                     put $ Foldr (IxSet.insert doc documents)
-- FIXME: if doc exists in Foldr, then update editables only

-- create types for event serialization
$(makeAcidic ''Foldr ['getDocument, 'addDocument])
