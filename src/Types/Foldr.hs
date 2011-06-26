{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances  #-}
module Types.Foldr where

import Happstack.Data                        (Default, Version, deriveAll)
import Happstack.State.ClockTime             () -- instance Data ClockTime
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Data
import Happstack.Data.Proxy
import Happstack.Data.IxSet                  (IxSet(..), ixSet, ixGen, Indexable, (@=))
import qualified Happstack.Data.IxSet        as IxSet
import Data.Int

newtype DocId = DocId Int64 deriving (Eq, Ord, Show, Data, Typeable)

-- |Document - Meta-information about a document and the editable content.
data Document = Document
  { author    :: String
    , id        :: DocId
    , title     :: String
--  , ctime     :: ClockTime
--  , mtime     :: ClockTime
    , editables :: [[(String, String)]] -- all changes to each editable FIXME: Should be HTML5 fragment
  } deriving (Show, Eq, Ord, Data, Typeable)

instance Indexable Document where
  empty = ixSet [ixGen (Proxy :: Proxy DocId)]

 -- |Foldr - A folder containing documents 
data Foldr = Foldr (IxSet Document) deriving (Show, Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 1 'base ''Foldr)
$(deriveSafeCopy 2 'base ''DocId)

instance (SafeCopy a, Ord a, Typeable a, IxSet.Indexable a) => SafeCopy (IxSet a) where
    putCopy ixSet = contain $ safePut (IxSet.toList ixSet)
    getCopy = contain $ IxSet.fromList <$> safeGet