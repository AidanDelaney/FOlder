{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
module State.Foldr
  (    Foldr(..)
     , DocId(..)
     , GetDocument(..)
     , AddDocument(..)
     , UpdateDocument(..)
     , GetNextDocId(..)
  ) where

import Data.Acid
import Control.Monad.Reader              (ask)
import Control.Monad.State               (get, put, modify)
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Data.IxSet              (IxSet, (@=))
--import Happstack.State                   (Dependencies, End, mkMethods)
import Types.Foldr

-- | get a 'Document' from a 'Foldr'
getDocument :: DocId -> Query Foldr (IxSet Document)
getDocument id
    = do Foldr documents next <- ask
         return $ documents @= (id)
  
-- | add a 'Document' to a 'Foldr'
addDocument :: Document -> Update Foldr ()
addDocument doc = do Foldr documents next <- get
                     put $ Foldr (IxSet.insert doc documents) (next+1)

-- | update a 'Document' in a 'Foldr'
updateDocument :: Document -> Update Foldr ()
updateDocument doc = do Foldr documents next <- get
                        put $ Foldr (IxSet.updateIx (docid doc) doc documents) next

getNextDocId :: Query Foldr (DocId)
getNextDocId = do (Foldr documents next) <- ask
                  return $ next

-- create types for event serialization
$(makeAcidic ''Foldr ['getDocument, 'addDocument, 'updateDocument, 'getNextDocId])
